{-# LANGUAGE DeriveAnyClass #-}
module CreditLine where
import Data.Aeson
import GHC.Generics
import Data.Text



data CLMCC =
  CLMCC
  { _allowedMcc :: Bool
  , _mcc :: [Text]}
  deriving (Show, Eq, Ord, Generic, Read , ToJSON , FromJSON)

-- instance FromJSON CLMCC where
--   parseJSON = withObject "CLMCC" $ \v ->  CLMCC
--     <$> v.: "_allowedMcc"
--     <*> v.: "_mcc"

-- instance ToJSON CLMCC where
--     toJSON (CLMCC _allowedMcc _mcc) =
--         object ["_allowedMcc" .= _allowedMcc, "_mcc" .= _mcc]

--     toEncoding (CLMCC _allowedMcc _mcc) =
--         pairs ("_allowedMcc" .= _allowedMcc <> "_mcc" .= _mcc)
