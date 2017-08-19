{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
  , module Miso.Types
  , module Miso.Router
  ) where

import Control.Monad.Trans.Control
import Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Lifted
import           Control.Monad
import           Data.IORef
import qualified Data.IORef.Lifted as LiftedIORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import qualified JavaScript.Object.Internal    as OI
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
import           Miso.Types
import           Miso.FFI

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: (Eq model, MonadBaseControl IO m)
  => App m model action
  -> model
  -> ((action -> IO ()) -> IO (IORef VTree))
  -> m b
common App {..} m getView = do
  -- init Notifier
  Notify {..} <- liftBase newNotify
  -- init EventWriter
  EventWriter {..} <- liftBase newEventWriter
  -- init empty Model
  modelRef <- liftBase (newIORef m)
  -- init empty actions
  actionsMVar <- newMVar S.empty
  -- init Subs
  liftBase $ forM_ subs $ \sub ->
    sub (readIORef modelRef) writeEvent
  -- init event application thread
  liftBase $ void . fork . forever $ do
    action <- getEvent
    modifyMVar_ actionsMVar $! \actions -> do
      pure (actions |> action)
    notify
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  liftBase . void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- liftBase $ getView writeEvent
  -- Begin listening for events in the virtual dom
  liftBase $ delegator viewRef events
  -- Process initial action of application
  liftBase $ writeEvent initialAction
  -- Program loop, blocking on SkipChan
  forever $ liftBase wait >> do
    -- Apply actions to model
    shouldDraw <- do
      modifyMVar actionsMVar $! \actions -> do
        (shouldDraw, effects) <- LiftedIORef.atomicModifyIORef' modelRef $! \oldModel ->
          let (newModel, effects) =
                foldl' (foldEffects (liftBase . writeEvent) update)
                  (oldModel, pure ()) actions
          in (newModel, (oldModel /= newModel, effects))
        effects
        pure (S.empty, shouldDraw)
    when shouldDraw $ liftBase $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readIORef modelRef
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      Just oldVTree `diff` Just newVTree
      atomicWriteIORef viewRef newVTree

-- | Runs an isomorphic miso application
-- Assumes the pre-rendered DOM is already present
miso :: (HasURI model, Eq model, MonadBaseControl IO m) => App m model action -> m ()
miso app@App{..} = do
  uri <- liftBase getCurrentURI
  let modelWithUri = setURI uri model
  common app model $ \writeEvent -> do
    let initialView = view modelWithUri
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    newIORef initialVTree

-- | Runs a miso application
startApp :: (Eq model, MonadBaseControl IO m) => App m model action -> m ()
startApp app@App {..} =
  common app model $ \writeEvent -> do
    let initialView = view model
    initialVTree <- flip runView writeEvent initialView
    Nothing `diff` (Just initialVTree)
    newIORef initialVTree

-- | Helper
foldEffects
  :: MonadBaseControl IO m
  => (action -> m ())
  -> (action -> model -> Effect m action model)
  -> (model, m ()) -> action -> (model, m ())
foldEffects sink update = \(model, as) action ->
  case update action model of
    Effect newModel effs -> (newModel, newAs)
      where
        newAs = as >> do
          forM_ effs $ \eff ->
            void $ fork (sink =<< eff)
