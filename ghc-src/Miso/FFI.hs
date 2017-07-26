{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( windowAddEventListener
   , windowRemoveEventListener
   , windowInnerHeight
   , windowInnerWidth
   , now
   , consoleLog
   , stringify
   , parse
   , copyDOMIntoVTree
   , item
   , jsvalToValue
   , delegateEvent
   ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.JSString
import           Data.Typeable
import           GHCJS.Marshal
import           GHCJS.Types
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types (JSM)
import           Language.Javascript.JSaddle.Value


newtype Callback a = Callback JSVal deriving Typeable
instance IsJSVal (Callback a)

-- | Convert JSVal to Maybe `Value`
jsvalToValue :: JSVal -> JSM (Maybe Value)
jsvalToValue = fromJSVal

-- | Adds event listener to window
windowAddEventListener :: JSString -> Callback (JSVal -> JSM ()) -> JSM ()
windowAddEventListener event (Callback cb) =
  void (jsg "window" ^. js2 "addEventListener" event cb)

-- | Removes event listener from window
windowRemoveEventListener :: JSString -> Callback (JSVal -> JSM ()) -> JSM ()
windowRemoveEventListener event (Callback cb) =
  void (jsg "window" ^. js2 "removeEventListener" event cb)

-- | Retrieves inner height
windowInnerHeight :: JSM Int
windowInnerHeight = fromJSValUnchecked =<< jsg "window" ^. js "innerHeight"

-- | Retrieves outer height
windowInnerWidth :: JSM Int
windowInnerWidth = fromJSValUnchecked =<< jsg "window" ^. js "innerWidth"

-- | Retrieve high performance time stamp
now :: JSM Double
now = fromJSValUnchecked =<< jsg "performance" ^. js0 "now"

-- | Console-logging
consoleLog :: JSVal -> JSM ()
consoleLog val' = void (jsg "console" ^. js1 "log" val')

-- | Converts a JS object into a JSON string
stringify' :: JSVal -> JSM JSString
stringify' = valToJSON

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> JSM JSString
{-# INLINE stringify #-}
stringify j = stringify' =<< toJSVal (toJSON j)

-- | Parses a JSString
parse :: FromJSON json => JSVal -> JSM json
{-# INLINE parse #-}
parse jval = do
  Just val' <- fromJSVal jval
  case fromJSON val' of
    Success x -> pure x
    Error y -> error y

-- | Indexing into a JS object
item :: JSVal -> JSString -> JSM JSVal
item this name = this ! name

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
copyDOMIntoVTree :: JSVal -> JSM ()
copyDOMIntoVTree vtree = void (jsg1 "copyDomIntoVTree" vtree)

-- | Event delegation FFI, routes events received on body through the virtual dom
-- Invokes event handler when found
delegateEvent :: JSVal -> Callback (JSM JSVal) -> JSM ()
delegateEvent event (Callback cb) =
  void (jsg2 "delegate" event cb)
