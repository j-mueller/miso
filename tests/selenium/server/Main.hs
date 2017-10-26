{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.Text as Text
import           Lucid
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid
import           System.Environment
import           Test.Hspec.WebDriver

browsers :: [Capabilities]
browsers = [chromeCaps]

type API = Get '[ HTML] (Html ()) :<|> "static" :> Raw

staticServer :: FilePath -> Server ("static" :> Raw)
staticServer dir = serveDirectoryFileServer dir

indexServer :: Server (Get '[HTML] (Html ()))
indexServer =
  pure . doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      with (script_ mempty) [src_ "static/all.js", defer_ ""]
      title_ "miso tests"
    body_ $ do "This is my body"

app :: String -> Application
app path = serve (Proxy :: Proxy API) (indexServer :<|> staticServer path)

main :: IO ()
main = do
  (path : args) <- getArgs
  withArgs args $ do
    forkIO $ run 3000 (app path)
    -- Give the server some time to start
    threadDelay (10 ^ (6 :: Int))
    hspec $
      describe "Can load index.html" $ do
        session "for index.html" $
          using browsers $ do
            it "opens the page" $ runWD $ openPage "http://localhost:3000/"
            it "has the correct title" $
              runWD $ getTitle `shouldReturn` "miso tests"
            describe "walk" $ do
              it "walks element with no children" $
                runWD $ do
                  b <-
                    executeJS
                      []
                      (Text.unlines
                         [ "var vtree = { type: 'vnode', tag: 'div', children: [] };"
                         , "var node = document.createElement('div');"
                         , "walk(vtree, node);"
                         , "return true;"
                         ])
                  b `shouldBe` True
              it "walks consecutive text nodes" $
                runWD $ do
                  b <-
                    executeJS
                      []
                      (Text.unlines
                         [ "var vtree = { type: 'vnode', tag: 'div', children: [ { type: 'vtext', text: 'first part'}, { type: 'vtext', text: '; second part'} ] };"
                         , "var node = document.createElement('div');"
                         , "var textNode = document.createTextNode('first part; second part');"
                         , "node.appendChild(textNode);"
                         , "walk(vtree, node);"
                         , "return true;"
                         ])
                  b `shouldBe` True
