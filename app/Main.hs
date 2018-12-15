{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

{- 
DeriveGeneric and GHC.Generics allow us to create FromJSON and ToJSON instances in our API. Data.Aeson is a library that provides the ability to convert to JSON.
-}
{- 
Spock is a Haskell based web framework, including middleware, routing, json, sessions, cookies, etc.
-}

-- data Person = Person 
--  { name :: Text
--  , age  :: Int
--  } deriving (Generic, Show)

-- instance ToJSON Person

-- instance FromJSON Person

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
 Person json -- json allows Persistent to generate readable JSON output.
  name Text
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
 pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
 spockCfg <- defaultSpockCfg () (PCPool pool) ()
 runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
 runSpock 8080 (spock spockCfg app)

app :: Api
app = do 
 get "people" $ do 
  json $ [ Person { personName = "Fry", personAge = 25 }
         , Person { personName = "Bender", personAge = 4 }
         ]
 post "people" $ do
  maybePerson <- jsonBody' :: ApiAction (Maybe Person)
  case maybePerson oft
    Nothing -> errorJson 1 "Failed to parse request body as Person"
    Just thePerson -> do
     newId <-- runSql $ insert thePerson
     json $ object ["result" .= String "success", "id" .= newId]

runSql 
 :: (HasSpock m, SpockConn m ~ SqlBackend) 
 => SqlPersistT (LoggingT IO) a -> m a
runSql action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn 

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
   object
   ["result" .= String "failure"
   ,"error" .= object ["code" .= code, "message" .= message]
   ]

  {- 

  curl -H "Content-Type: application/json" -d { "name": "Bart", "age": 10 } localhost:8080/people
  
  ^ Currently unable to use curl to check if post method works as curl is native to Linux and I'm on Windows.
  Our Api data type represents the apps configuration. 
  ApiActions type is similar and represents actions in our application, 
  which are functions performed by route matches (get). 
  Later we'll use it to explicitly declare types for use in actions.
  -}




