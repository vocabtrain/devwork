{-# OPTIONS_GHC -fno-warn-orphans -XFlexibleInstances #-}
module Application
	( makeApplication
	, getApplicationDev
	, makeFoundation
	) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Dominik
import Handler.Beamer
import Handler.Root
import Handler.Tatoeba
import Handler.Vocabtrain
import Handler.VocabtrainMobile

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp
--mkYesodSubDispatch "DominikSub" [ClassP ''Yesod [VarT $ mkName "master"]] resourcesDominikSub

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
	foundation <- makeFoundation conf
	app <- toWaiAppPlain foundation
	return $ logWare app
  where
	logWare   = if development then logStdoutDev
							   else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
	manager <- newManager def
	s <- staticSite
	dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
			  Database.Persist.Store.loadConfig >>=
			  Database.Persist.Store.applyEnv
	p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
	logger <- mkLogger True stdout
	let foundation = App conf s p manager dbconf logger

	runLoggingT
		(Database.Persist.Store.runPool dbconf (mapM_ runMigration [migrateServer]) p)
		(messageLoggerSource foundation logger)
	--Database.Persist.Store.runPool dbconf (mapM_ runMigration [migrateCore, migrateVocabtrain, migrateVocabtrainServer]) p
--	return $ App conf s p manager dbconf --DominikSub
	return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
	defaultDevelApp loader makeApplication
  where
	loader = loadConfig (configSettings Development)
		{ csParseExtra = parseExtra
		}
