{-# LANGUAGE FlexibleContexts #-}
module Util.Json where

import Data.Aeson
    ( genericParseJSON,
      defaultOptions,
      genericToJSON,
      GFromJSON,
      Zero,
      Options(omitNothingFields, fieldLabelModifier, sumEncoding),
      Value,
      GToJSON', SumEncoding (UntaggedValue) )
import Data.Aeson.Types ( Parser )
import GHC.Generics ( Generic(Rep) )

dropToJSON ::  (Generic a, GToJSON' Value Zero (Rep a)) => 
  Int -> a -> Value
dropToJSON prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  , sumEncoding = UntaggedValue
}

dropParseJSON :: (Generic a, GFromJSON Zero (Rep a)) =>
    Int -> Value -> Parser a
dropParseJSON prefix = genericParseJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , sumEncoding = UntaggedValue 
}