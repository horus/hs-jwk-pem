{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString as B (getContents)
import Data.ByteString.Builder (lazyByteString, stringUtf8)
import Lib
import Network.HTTP.Types (hContentType, status200, status400)
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
    . either err ok
    . encodePEMWith encoder
    =<< consumeRequestBodyLazy req
  where
    encoder
      | any ((== "rsa") . fst) $ queryString req = encodePKCS1
      | otherwise = encodeX509
    err = responseBuilder status400 [(hContentType, "text/plain")] . stringUtf8
    ok = responseBuilder status200 [(hContentType, "application/x-pem-file")] . lazyByteString
