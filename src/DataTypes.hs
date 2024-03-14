{-# LANGUAGE DeriveAnyClass #-}
module DataTypes where

import qualified Data.Text as DT
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import GHC.Generics

newtype UserName = UserName DT.Text deriving (Show, Eq)
newtype PetName = PetName DT.Text deriving (Show, Eq)

-- | A type alias to improve the readability of our types.
-- An inventory is a HashMap with a Text key and Item value.
type Inventory = HM.HashMap DT.Text Item

-- | A User record.
data User
  = User
  { _userName :: UserName
  , _userScore :: Int
  , _userPet :: Maybe Pet
  , _userInventory :: Inventory
  } deriving (Show, Eq)

newtype Pet
  = Pet
  { _petName :: PetName } deriving (Show, Eq)

-- | An Item record.
data Item
  = Item
  { _itemValue :: Int
  , _itemWeight :: Int
  } deriving (Show, Eq)

newtype StorageError = StorageError DT.Text deriving (Eq, Show)

-- WebError wraps storage Error
data WebError
  = WebTextError DT.Text
  | WebStorageError StorageError
  deriving (Eq, Show)

data CBSErrorCode = 
    RemitterError RemitterErrorCode 
  | BeneficiaryError BeneficiaryErrorCode 
  | CommonError CommonErrorCode 
  | SomeError DT.Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Read)


data RemitterErrorCode =
    Z9 
  | YE
  | ZX
  | B1
  | Z8
  | Z7
  | K1
  | XB
  | YA 
  | XL
  | XH
  | XY
  | AJ
  | B7
  | SP
  | XN
  | XR 
  | ZM
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)
data BeneficiaryErrorCode = 
    ZY
  | YF
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data CommonErrorCode =
    B3
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

