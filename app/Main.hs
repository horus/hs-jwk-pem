{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString as B (putStr)
import Data.ByteString.Builder (byteString, stringUtf8)
import Data.ByteString.Lazy as L (getContents)
import Lib (encodePEM)
import Network.HTTP.Types (status200, status400)
import Network.Wai
import Network.Wai.Handler.Warp as Warp (run)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    ("web" : _) -> Warp.run 9000 app
    _ -> either putStrLn B.putStr . encodePEM =<< L.getContents

app :: Application
app req respond =
  respond
    . either (resp status400 . stringUtf8) (resp status200 . byteString)
    . encodePEM
    =<< consumeRequestBodyLazy req
  where
    resp status content =
      responseStream status [] $ \write flush -> write content >> flush
