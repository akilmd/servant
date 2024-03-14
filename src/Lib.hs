{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.List ( find )
import Data.Aeson
import GHC.Generics

--type API = "greet" :> Capture "name" String :> Get '[PlainText] String --2
--type API = "search" :> QueryParam "term" String :> Get '[PlainText] String


type LibraryAPI = "books" :> QueryParam "title" String :> Get '[JSON] [Book]
             :<|> "books" :> Capture "bookid" Int :> "borrow" :> Post '[JSON] Book
             :<|> "books" :> Capture "bookid" Int :> "return" :> Post '[JSON] Book


startApp :: IO()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy LibraryAPI
api = Proxy

-- server :: Server API 
-- server = search 
-- greet 
-- (return "abjsballnl" -->1) 

-- search :: Maybe String -> Handler String 
-- search Nothing       = return [] 
-- search (Just term)   = return $ "Hello, " <> term <> "!" 

server :: Server LibraryAPI
server = searchBooks
     :<|> borrowBook
     :<|> returnBook

data Book = Book {bookId :: Int , bookStatus :: String , bookTitle :: String }
    deriving (Generic, Show, Eq)
instance ToJSON Book
instance FromJSON Book

library :: [Book]
library = [Book 1 "Available" "new1", Book 2 "Available" "new2"]

searchBooks :: Maybe String -> Handler [Book]
searchBooks Nothing      = return library
searchBooks (Just title) = return $ filter (\book -> bookTitle book == title) library

borrowBook :: Int -> Handler Book
borrowBook bId = do
    let bookPresent = find (\book -> bookId book == bId) library
    case bookPresent of
        Nothing -> throwError err404
        Just book -> return book { bookStatus = "Borrowed" }

returnBook :: Int -> Handler Book
returnBook id = do
    let bookPresent = find (\book -> bookId book == id && bookStatus book == "Borrowed") library
    case bookPresent of
        Nothing   -> throwError err404
        Just x -> return x {bookStatus = "Available"}
