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
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (toASN1)
import Data.Aeson as A
import Data.ByteString qualified as S
import Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Base64.URL (decodeUnpadded)
import Data.ByteString.Builder (Builder, byteString, charUtf8, hPutBuilder, toLazyByteString)
import Data.ByteString.Lazy qualified as L (ByteString)
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

outputPEM :: S.ByteString -> IO ()
outputPEM = either putStrLn (hPutBuilder stdout) . eitherPEM
  where
    eitherPEM = fmap buildPEM . A.eitherDecodeStrict @Jwk'

buildPEM :: Jwk' -> Builder
buildPEM = pemBuilder . asn1DER . pubkey

encodePEM :: L.ByteString -> Either String L.ByteString
encodePEM = fmap (toLazyByteString . buildPEM) . A.eitherDecode @Jwk'

pemBuilder :: S.ByteString -> Builder
pemBuilder content = splitBuild content begin <> end
  where
    begin = "-----BEGIN RSA PUBLIC KEY-----\n"
    end = "-----END RSA PUBLIC KEY-----\n"
    base64 line = byteString (B64.encode line) <> charUtf8 '\n'
    splitBuild bs builder
      | S.length bs <= 48 = builder <> base64 bs
      | otherwise =
          let (x, y) = S.splitAt 48 bs
           in splitBuild y (builder <> base64 x)

asn1DER :: PubKey -> S.ByteString
asn1DER = encodeASN1' DER . flip toASN1 []

size :: Integer -> Int -> Int
size !n !i
  | 2 ^ (i * 8) > n = i
  | otherwise = size n (i + 1)
