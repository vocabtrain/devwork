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
import qualified Database.Persist.Store
import Yesod
import Data.Text (Text)
import Yesod.Default.Config
import Database.Persist.Postgresql

getPostgresConnectionString :: FilePath -> DefaultEnv -> IO ConnectionString
getPostgresConnectionString config env = do
	dbconf <- withYamlEnvironment config env
		Database.Persist.Store.loadConfig >>=
		Database.Persist.Store.applyEnv :: IO PostgresConf
	return $ pgConnStr dbconf
