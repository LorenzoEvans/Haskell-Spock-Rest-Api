{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

{- 
DeriveGeneric and GHC.Generics allow us to create FromJSON and ToJSON instances in our API. Data.Aeson is a library that provides the ability to convert to JSON.
-}
{- 
Spock is a Haskell based web framework, including middleware, routing, json, sessions, cookies, etc.
-}

data Person = Person 
 { name :: Text
 , age  :: Int
 } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do 
 get "people" $ do 
  json $ [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]
 post "people" $ do
  thePerson <- jsonBody' :: ApiAction Person
  text $ "Parsed: " <> pack (show thePerson)

  {- 

  curl -H "Content-Type: application/json" -d { "name": "Bart", "age": 10 } localhost:8080/people
  
  ^ Currently unable to use curl to check if post method works as curl is native to Linux and I'm on Windows.
  Our Api data type represents the apps configuration. 
  ApiActions type is similar and represents actions in our application, 
  which are functions performed by route matches (get). 
  Later we'll use it to explicitly declare types for use in actions.
  -}




