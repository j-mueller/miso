-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.XHR
, module Miso.Effect.DOM
, Effect (..)
, noEff
, (<#)
, (#>)
) where

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of actions. Each action
-- is run in a new thread so there is no risk of accidentally
-- blocking the application.
data Effect m action model
  = Effect model [m action]

instance Functor (Effect m action) where
  fmap f (Effect m acts) = Effect (f m) acts

instance Applicative (Effect m action) where
  pure m = Effect m []
  Effect fModel fActs <*> Effect xModel xActs = Effect (fModel xModel) (fActs ++ xActs)

instance Monad (Effect m action) where
  return = pure
  Effect m acts >>= f =
    case f m of
      Effect m' acts' -> Effect m' (acts ++ acts')

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect m action model
noEff m = Effect m []

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> m action -> Effect m action model
(<#) m a = Effect m [a]

-- | `Effect` smart constructor, flipped
(#>) :: m action -> model -> Effect m action model
(#>) = flip (<#)
