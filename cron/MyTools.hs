{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module MyTools
	where


import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Prelude
import System.IO 
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Environment
import qualified Database.Persist
import Yesod
import Data.Text (Text)
import Yesod.Default.Config
import Database.Persist.Postgresql


import qualified Database.PostgreSQL.Simple as PG
import Data.Aeson


getPostgresConnectionString :: FilePath -> DefaultEnv -> IO ConnectionString
getPostgresConnectionString config env = do
	dbconf <- withYamlEnvironment config env
		Database.Persist.loadConfig >>=
		Database.Persist.applyEnv :: IO PostgresConf
	return $ pgConnStr dbconf


getPostgresConnectionConfig :: FilePath -> DefaultEnv -> IO PG.ConnectInfo
getPostgresConnectionConfig config env = withYamlEnvironment config env loadConfig
	where
	loadConfig (Object o) = do
		database <- o .: "database"
		host	 <- o .: "host"
		port	 <- o .:? "port" .!= 5432
		user	 <- o .: "user"
		password <- o .: "password"
		return PG.ConnectInfo
				   { PG.connectHost	 = host
				   , PG.connectPort	 = port
				   , PG.connectUser	 = user
				   , PG.connectPassword = password
				   , PG.connectDatabase = database
				   }
	loadConfig _ = mzero

