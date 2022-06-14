{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Lib (encodePEM) where

import Control.Monad (guard)
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.RSA (PublicKey (PublicKey))
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (toASN1)
import Data.Aeson as A
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L (ByteString)
import Data.PEM (PEM (..), pemWriteBS)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.X509 (PubKey (PubKeyRSA))

newtype N = N Integer

instance FromJSON N where
  parseJSON = withText "N" $ \t ->
    case convertFromBase @_ @ByteString Base64URLUnpadded (encodeUtf8 t) of
      Right bs -> return $ N (os2ip bs)
      _ -> fail "could not decode n"

newtype E = E Integer

instance FromJSON E where
  parseJSON = withText "E" $ \case
    "AQAB" -> return $ E 65537
    "AAEAAQ" -> return $ E 65537
    e -> fail $ "unrecognized exponent " ++ show e

data Jwk = Jwk
  { kty :: Text,
    n :: N,
    e :: E
    {--
        kid :: Maybe Text,
        alg :: Maybe Text,
        use :: Maybe Text
    --}
  }

instance A.FromJSON Jwk where
  parseJSON = withObject "JWK" $ \o -> do
    kty <- o .: "kty"
    guard (kty == "RSA")
    n <- o .: "n"
    e <- o .: "e"
    {--
        kid <- o .:? "kid"
        alg <- o .:? "alg"
        use <- o .:? "use"
    --}
    return Jwk {..}

buildPEM :: Jwk -> ByteString
buildPEM jwk = pemWriteBS $ PEM "RSA PUBLIC KEY" [] $ asn1DER (rsaPubKey jwk.n jwk.e)

asn1DER :: PubKey -> ByteString
asn1DER = encodeASN1' DER . flip toASN1 []

rsaPubKey :: N -> E -> PubKey
rsaPubKey (N n) (E e) = PubKeyRSA $ PublicKey (size n 1) n e

size :: Integer -> Int -> Int
size !n !i
  | 2 ^ (i * 8) > n = i
  | otherwise = size n (i + 1)

encodePEM :: L.ByteString -> Either String ByteString
encodePEM = fmap buildPEM . A.eitherDecode @Jwk
