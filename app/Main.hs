{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString as B (getContents)
import Data.ByteString.Builder (lazyByteString, stringUtf8)
import Lib
import Network.HTTP.Types (status200, status400)
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    ("web" : _) -> Warp.run 9000 (logStdout app)
    _ -> B.getContents >>= outputPEM

app :: Wai.Application
app req respond =
  respond
    . either (resp status400 . stringUtf8) (resp status200 . lazyByteString)
    . encodePEM
    =<< consumeRequestBodyLazy req
  where
    resp status content =
      responseStream status [] $ \write flush -> write content >> flush
