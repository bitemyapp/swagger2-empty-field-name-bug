{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL8
import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Swagger.Internal.TypeShape
import           Data.Aeson.Encode.Pretty   (encodePretty)

data TestBadRecord = TestBadRecord {
    blah :: String
  , woot :: Int
  } deriving (Eq, Generic, Show)

instance ToSchema TestBadRecord where
  declareNamedSchema proxy =
      genericDeclareNamedSchema
      defaultSchemaOptions {
          fieldLabelModifier = drop (length "testBadRecord")
        , constructorTagModifier = id
      }
      proxy

data TestGoodRecord = TestGoodRecord {
    testGoodRecordBlah :: String
  , testGoodRecordWoot :: Int
  } deriving (Eq, Generic, Show)

instance ToSchema TestGoodRecord where
  declareNamedSchema proxy =
      genericDeclareNamedSchema
      defaultSchemaOptions {
          fieldLabelModifier = drop (length "testGoodRecord")
        , constructorTagModifier = id
      }
      proxy

main :: IO ()
main = do
  badSchema
  goodSchema
  where
    dumpSchema proxy =
      BL8.putStrLn
      $ encodePretty
      $ toSchema proxy
    badSchema =
      dumpSchema (Proxy :: Proxy TestBadRecord)
    goodSchema =
      dumpSchema (Proxy :: Proxy TestGoodRecord)
