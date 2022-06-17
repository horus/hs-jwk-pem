{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Monad (when)
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.RSA qualified as RSA (PublicKey (PublicKey))
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1)
import Data.ASN1.Types (toASN1)
import Data.Aeson as A
import Data.ByteString.Base64.Lazy as B64 (encode)
import Data.ByteString.Base64.URL (decodeUnpadded)
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as L (ByteString, length, splitAt)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.X509 (PubKey (PubKeyRSA))
import System.IO (stdout)

newtype N = N Integer

instance FromJSON N where
  parseJSON = withText "N" $ \t ->
    let decoded = decodeUnpadded (encodeUtf8 t)
     in either (const $ fail "could not decode n") (return . N . os2ip) decoded

newtype E = E Integer

instance FromJSON E where
  parseJSON = withText "E" $ \e ->
    if e == "AQAB" || e == "AAEAAQ"
      then return $ E 65537
      else fail $ "unrecognized exponent " ++ show e

newtype Jwk' = Jwk' {pubkey :: PubKey}

instance A.FromJSON Jwk' where
  parseJSON = withObject "JWK" $ \o -> do
    kty :: Text <- o .: "kty"
    when (kty /= "RSA") $ fail $ "unsupported key type: " ++ show kty
    N n <- o .: "n"
    E e <- o .: "e"
    return $ Jwk' $ PubKeyRSA $ RSA.PublicKey (size n 1) n e

pemOutput :: L.ByteString -> IO ()
pemOutput = either putStrLn (hPutBuilder stdout) . fmap buildPEM' . A.eitherDecode @Jwk'

buildPEM :: Jwk' -> L.ByteString
buildPEM = toLazyByteString . buildPEM'

buildPEM' :: Jwk' -> Builder
buildPEM' = pemBuilder . asn1DER . pubkey

pemBuilder :: L.ByteString -> Builder
pemBuilder content = splitBuild content begin <> end
  where
    begin = "-----BEGIN RSA PUBLIC KEY-----\n"
    end = "-----END RSA PUBLIC KEY-----\n"
    base64 line = lazyByteString (B64.encode line) <> charUtf8 '\n'
    splitBuild bs builder
      | L.length bs <= 48 = builder <> base64 bs
      | otherwise =
          let (x, y) = L.splitAt 48 bs
           in splitBuild y (builder <> base64 x)

asn1DER :: PubKey -> L.ByteString
asn1DER = encodeASN1 DER . flip toASN1 []

size :: Integer -> Int -> Int
size !n !i
  | 2 ^ (i * 8) > n = i
  | otherwise = size n (i + 1)

encodePEM :: L.ByteString -> Either String L.ByteString
encodePEM = fmap buildPEM . A.eitherDecode @Jwk'
