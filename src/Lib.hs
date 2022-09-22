{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad (when)
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.RSA qualified as RSA (PublicKey (..))
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (Sequence), toASN1)
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
outputPEM = either putStrLn (hPutBuilder stdout) . pem
  where
    pem = fmap (encodeX509 . pubkey) . A.eitherDecodeStrict

encodeX509 :: PubKey -> Builder
encodeX509 = pemBuilder "-----BEGIN PUBLIC KEY-----\n" "-----END PUBLIC KEY-----\n" . asn1DER
  where
    asn1DER = encodeASN1' DER . flip toASN1 []

encodePKCS1 :: PubKey -> Builder
encodePKCS1 = pemBuilder "-----BEGIN RSA PUBLIC KEY-----\n" "-----END RSA PUBLIC KEY-----\n" . asn1DER
  where
    asn1DER (PubKeyRSA rsa) =
      encodeASN1'
        DER
        [ Start Sequence,
          IntVal rsa.public_n,
          IntVal rsa.public_e,
          End Sequence
        ]
    asn1DER _ = "" -- RSA Public key ONLY

encodePEMWith :: (PubKey -> Builder) -> L.ByteString -> Either String L.ByteString
encodePEMWith enc = fmap (toLazyByteString . enc . pubkey) . A.eitherDecode

pemBuilder :: Builder -> Builder -> S.ByteString -> Builder
pemBuilder begin end content = split content begin <> end
  where
    base64 line = byteString (B64.encode line) <> charUtf8 '\n'
    split bs builder
      | S.length bs <= 48 = builder <> base64 bs
      | otherwise =
          let (x, y) = S.splitAt 48 bs
           in split y (builder <> base64 x)

size :: Integer -> Int -> Int
size !n !i
  | 2 ^ (i * 8) > n = i
  | otherwise = size n (i + 1)
