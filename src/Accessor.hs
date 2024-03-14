{-# LANGUAGE TemplateHaskell #-}

module Accessor where

import Control.Lens
import qualified DataTypes as T
import qualified CreditLine as CL

makeFieldsNoPrefix ''T.User
makeFieldsNoPrefix ''CL.CLMCC
makeFieldsNoPrefix ''T.Pet
makeFieldsNoPrefix ''T.Item