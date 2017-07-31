{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Router
  ( runRoute
  , RoutingError (..)
  , HasURI (..)
  , getURI
  , setURI
  , makeLens
  ) where

import qualified Control.Applicative as A
import qualified Data.ByteString.Char8 as BS
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.URI
import           Servant.API
import           Web.HttpApiData

import           Miso.Html             hiding (text)

-- | Router terminator.
-- The 'HasRouter' instance for 'View' finalizes the router.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "bookId" Int :> View

-- | 'Location' is used to split the path and query of a URI into components.
data Location = Location
  { locPath  :: [Text]
  , locQuery :: Query
  } deriving (Show, Eq, Ord)

-- | When routing, the router may fail to match a location.
-- Either this is an unrecoverable failure,
-- such as failing to parse a query parameter,
-- or it is recoverable by trying another path.
data RoutingError
  = Fail
  | FailFatal
  deriving (Show, Eq, Ord)

-- | A 'Router' contains the information necessary to execute a handler.
data Router a where
  RChoice       :: Router a -> Router a -> Router a
  RCapture      :: FromHttpApiData x => (x -> Router a) -> Router a
  RQueryParam   :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> (Maybe x -> Router a) -> Router a
  RQueryParams  :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> ([x] -> Router a) -> Router a
  RQueryFlag    :: KnownSymbol sym
                   => Proxy sym -> (Bool -> Router a) -> Router a
  RPath         :: KnownSymbol sym => Proxy sym -> Router a -> Router a
  RPage         :: a -> Router a

-- | This is similar to the @HasServer@ class from @servant-server@.
-- It is the class responsible for making API combinators routable.
-- 'RouteT' is used to build up the handler types.
-- 'Router' is returned, to be interpretted by 'routeLoc'.
class HasRouter model layout where
  -- | A route handler.
  type RouteT model layout a :: *
  -- | Transform a route handler into a 'Router'.
  route :: Proxy layout -> Proxy a -> RouteT model layout a -> model -> Router a

-- | Alternative
instance (HasRouter m x, HasRouter m y) => HasRouter m (x :<|> y) where
  type RouteT m (x :<|> y) a = RouteT m x a :<|> RouteT m y a
  route _ (a :: Proxy a) ((x :: RouteT m x a) :<|> (y :: RouteT m y a)) m
    = RChoice (route (Proxy :: Proxy x) a x m) (route (Proxy :: Proxy y) a y m)

-- | Capture
instance (HasRouter m sublayout, FromHttpApiData x) =>
  HasRouter m (Capture sym x :> sublayout) where
  type RouteT m (Capture sym x :> sublayout) a = x -> RouteT m sublayout a
  route _ a f m = RCapture (\x -> route (Proxy :: Proxy sublayout) a (f x) m)

-- | QueryParam
instance (HasRouter m sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter m (QueryParam sym x :> sublayout) where
  type RouteT m (QueryParam sym x :> sublayout) a = Maybe x -> RouteT m sublayout a
  route _ a f m = RQueryParam (Proxy :: Proxy sym)
    (\x -> route (Proxy :: Proxy sublayout) a (f x) m)

-- | QueryParams
instance (HasRouter m sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter m (QueryParams sym x :> sublayout) where
  type RouteT m (QueryParams sym x :> sublayout) a = [x] -> RouteT m sublayout a
  route _ a f m = RQueryParams
    (Proxy :: Proxy sym)
    (\x -> route (Proxy :: Proxy sublayout) a (f x) m)

-- | QueryFlag
instance (HasRouter m sublayout, KnownSymbol sym)
         => HasRouter m (QueryFlag sym :> sublayout) where
  type RouteT m (QueryFlag sym :> sublayout) a = Bool -> RouteT m sublayout a
  route _ a f m = RQueryFlag
    (Proxy :: Proxy sym)
    (\x -> route (Proxy :: Proxy sublayout) a (f x) m)

-- | Path
instance (HasRouter m sublayout, KnownSymbol path)
         => HasRouter m (path :> sublayout) where
  type RouteT m (path :> sublayout) a = RouteT m sublayout a
  route _ a page m = RPath
    (Proxy :: Proxy path)
    (route (Proxy :: Proxy sublayout) a page m)

-- | View
instance HasRouter m (View a) where
  type RouteT m (View a) x = m -> x
  route _ _ a m = RPage (a m)

-- | Use a handler to route a 'Location'.
-- Normally 'runRoute' should be used instead, unless you want custom
-- handling of string failing to parse as 'URI'.
runRouteLoc :: forall m layout a. HasRouter m layout
            => Location -> Proxy layout -> RouteT m layout a -> m -> Either RoutingError a
runRouteLoc loc layout page m =
  let routing = route layout (Proxy :: Proxy a) page m
  in routeLoc loc routing m

-- | Use a handler to route a location, represented as a 'String'.
-- All handlers must, in the end, return @m a@.
-- 'routeLoc' will choose a route and return its result.
runRoute
  :: (HasURI m, HasRouter m layout)
  => Proxy layout
  -> RouteT m layout a
  -> m
  -> Either RoutingError a
runRoute layout page m = runRouteLoc (uriToLocation (getURI m)) layout page m

-- | Use a computed 'Router' to route a 'Location'.
routeLoc :: Location -> Router a -> m -> Either RoutingError a
routeLoc loc r m = case r of
  RChoice a b -> do
    case routeLoc loc a m of
      Left Fail -> routeLoc loc b m
      Left FailFatal -> Left FailFatal
      Right x -> Right x
  RCapture f -> case locPath loc of
    [] -> Left Fail
    capture:paths ->
      case parseUrlPieceMaybe capture of
        Nothing -> Left FailFatal
        Just x -> routeLoc loc { locPath = paths } (f x) m
  RQueryParam sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc (f Nothing) m
    Just Nothing -> Left FailFatal
    Just (Just text) -> case parseQueryParamMaybe (decodeUtf8 text) of
      Nothing -> Left FailFatal
      Just x -> routeLoc loc (f (Just x)) m
  RQueryParams sym f -> maybe (Left FailFatal) (\x -> routeLoc loc (f x) m) $ do
    ps <- sequence $ snd <$> Prelude.filter
      (\(k, _) -> k == BS.pack (symbolVal sym)) (locQuery loc)
    sequence $ (parseQueryParamMaybe . decodeUtf8) <$> ps
  RQueryFlag sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc (f False) m
    Just Nothing -> routeLoc loc (f True) m
    Just (Just _) -> Left FailFatal
  RPath sym a -> case locPath loc of
    [] -> Left Fail
    p:paths -> if p == T.pack (symbolVal sym)
      then routeLoc (loc { locPath = paths }) a m
      else Left Fail
  RPage a ->
    case locPath loc of
      [] -> Right a
      _ -> Left Fail

-- | Convert a 'URI' to a 'Location'.
uriToLocation :: URI -> Location
uriToLocation uri = Location
  { locPath = decodePathSegments $ BS.pack (uriPath uri)
  , locQuery = parseQuery $ BS.pack (uriQuery uri)
  }

class HasURI m where lensURI :: Lens' m URI

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
type Getting r s a = (a -> Const r a) -> (s -> Const r s)

newtype Const r a = Const { runConst :: r }
  deriving Functor

newtype Id a = Id { runId :: a }
  deriving (Functor)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

get :: Getting a s a -> s -> a
get l = \ s -> runConst (l Const s)

set :: Lens s t a b -> b -> s -> t
set l b = \ s -> runId (l (\ _ -> A.pure b) s)

makeLens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
makeLens get' upd = \ f s -> upd s `fmap` f (get' s)

getURI :: HasURI m => m -> URI
getURI = get lensURI

setURI :: HasURI m => URI -> m -> m
setURI m u = set lensURI m u





