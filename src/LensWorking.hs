{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}

module LensWorking where

import qualified Data.Text as DT
import qualified Data.HashMap.Strict as HM
import Prelude
import qualified CreditLine as CL
import DataTypes
import qualified Accessor as A
import Data.Maybe
import Text.Read
import Control.Lens

viewExamples :: IO()
viewExamples = do
  let bob = User (UserName "Bob") 42 (Just (Pet (PetName "tomy") ) ) HM.empty
  let clmcc = CL.CLMCC True ["01" , "02"]
  -- view or ^. used to view tghe value
  print  $ bob ^. A.userName
  -- print $ bob ^. A.userScore
  -- -- that ^? is used for Just type we can take the whole Just type by _just and it always return a just type.
  -- print $ bob ^? A.userPet . _Just . A.petName -- Just (Pet {_petName = PetName "tomy"})
  -- -- set or .~ it used to set the value
  -- print $ bob
  --  & A.userName .~ UserName "Akil"
  --  & A.userScore .~ 20
  --  & A.userPet ?~   Pet (PetName "tommy")
  --  -- that ?. is used for Just return type.
  --  -- over or %~ is use to update the value
  -- print $ bob & A.userScore %~ (\sc -> sc+1 )
  -- print $ bob & A.userScore %~ (+1)
  -- print $ bob & A.userScore +~ 1
  -- let bobWithTommy = bob & A.userPet . _Just . A.petName %~
  --      (\ (PetName n)-> PetName ( DT.pack "Y" <> n) )
  -- print $ bobWithTommy ^? A.userPet . _Just . A.petName

-- >>> viewExamples
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

moreOverExamples :: IO()
moreOverExamples = do
  let badStorageResponse =
        Left (StorageError "fail!!") :: Either StorageError DT.Text
      goodStorageResponse =
        Right "Got the data " :: Either StorageError DT.Text
  print badStorageResponse
  print $ badStorageResponse & _Left %~ (\ err -> WebStorageError err)
  print $ badStorageResponse & _Left %~ WebStorageError
  either
    (const $ print badStorageResponse)
    print
    goodStorageResponse

-- >>> moreOverExamples
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

-- at and ix

atIxExamples :: IO()
atIxExamples = do
  -- Yep, you can use apostrophes in var names. Not that you should...
  let bob'sInventory = HM.fromList [ ("gold", Item 99 10) --Item is defined in start
                                    , ("silver", Item 10 9)
                                    ]
      bob = User (UserName "bobMarley") 42 Nothing bob'sInventory
  print "bob's Gold Value : "
  print $ bob ^? A.userInventory . at "gold" . _Just  . A.itemValue -- viewing a value from hashMap Just 99
  print $ bob ^? A.userInventory . ix "gold" . A.itemValue

  print "Bob's finds a diamond "
  let bobAddDiamond' = bob & A.userInventory . at "diamond"  .~ Just (Item 20 30)
      bobAddDiamond = bob & A.userInventory . at "diamond"  ?~ Item 20 30 -- insering value to hashmap

  print $ bobAddDiamond ^? A.userInventory . ix "diamond" . A.itemValue
  let bobNoGold = bobAddDiamond' & A.userName .~ UserName "Sad bob"
                      & A.userScore +~ 6
                      & A.userInventory . at "gold" .~ Nothing -- deleting the key and value  from HashMap
  print "bob loosed his gold"
  print $ bobNoGold ^. A.userInventory . at "gold"
  print $ bobNoGold ^? A.userInventory . at "gold"
  print $ bobNoGold ^? A.userInventory . ix "gold" -- so ix shoulkd only work with ^? as it doesnt return maybe so to avoid exception not found

  let defaultItem = Item 0 0
  print "returning Default value if not found"
  print $ bobNoGold ^. A.userInventory . at "gold"  . non defaultItem . A.itemValue -- used of non
  print $ bobNoGold ^. A.userInventory -- . at "gold" . _Just . itemValue

-- >>> atIxExamples
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

toListOfExamples :: IO ()
toListOfExamples = do
  let inventory = HM.fromList [ ("gold", Item 99 10)
                          , ("silver", Item 50 70)
                          ]
      bob = User (UserName "bob") 42 Nothing inventory
  print $ bob ^.. A.userInventory -- it returns the whole items in form of list
  print $ bob ^.. A.userInventory . folded  -- it return the list of values fromHashMap
  print $ bob ^.. A.userInventory . folded . asIndex -- it will return the list of keys of map but in 0-1 format prettyr useless
  print $ bob ^.. A.userInventory . ifolded . asIndex -- it will return the list of keys in original format
  print $ bob ^.. A.userInventory . folded . filtered (\item -> (item ^. A.itemValue) > 50)
  print $ bob ^.. A.userInventory  . traverse
  print $ bob ^.. A.userInventory  . traverse . A.itemValue

-- >>> toListOfExamples

hasGotcha :: IO ()
hasGotcha = do
  let bob = User (UserName "bob") 42 Nothing HM.empty

  print "Has bob gold in his inventory?"
  print $ has (A.userInventory . at "gold") bob -- it always return True for Lens

-- >>> hasGotcha

hasGotchaIx :: IO ()
hasGotchaIx = do
  let bob = User (UserName "bob") 42 Nothing HM.empty
  print "Has bob gold in his inventory?"
  print $ has (A.userInventory . ix "gold") bob
  -- False

  let richBob = User (UserName "Rich bob") 42 Nothing
                  $ HM.fromList [("gold", Item 10 10)
                  , ("silver", Item 1 1)]
  print "Has Rich bob gold in his inventory?"
  print $ has (A.userInventory . ix "silver") richBob
-- >>> hasGotchaIx


justPractice :: IO()
justPractice = do
  let inventory = HM.fromList [ ("gold" :: String, "33")
                          , ("silver", "44")
                          ]
  print $ fromMaybe 500 . (>>= readMaybe) $ HM.lookup "gold" inventory

-- >>> justPractice
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')


errorConversion :: IO()
errorConversion = do
  let responseCode :: DT.Text = "ZM"
  print (readMaybe. DT.unpack $ responseCode :: Maybe CBSErrorCode)


-- >>> errorConversion
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

-- randomNumber :: Int -> Int -> IO Int
-- randomNumber a b = randomRIO (a, b)

-- generateotp :: IO ()
-- generateotp = do
--   otp <- DT.pack . concatMap show <$> replicateM 5 (randomNumber 0 9)
--   print otp

-- >>> generateotp
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')
