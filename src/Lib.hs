{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib (encodePEM) where

import Control.Monad (when)
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.RSA qualified as RSA (PublicKey (PublicKey))
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (toASN1)
import Data.Aeson as A
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L (ByteString)
import Data.PEM (PEM (..), pemWriteLBS)
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

buildPEM :: Jwk' -> L.ByteString
buildPEM = pemWriteLBS . PEM "RSA PUBLIC KEY" [] . asn1DER . pubkey

asn1DER :: PubKey -> ByteString
asn1DER = encodeASN1' DER . flip toASN1 []

size :: Integer -> Int -> Int
size !n !i
  | 2 ^ (i * 8) > n = i
  | otherwise = size n (i + 1)

encodePEM :: L.ByteString -> Either String L.ByteString
encodePEM = fmap buildPEM . A.eitherDecode @Jwk'
