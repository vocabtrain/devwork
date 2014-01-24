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
import Network.Wai.Middleware.RequestLogger
	( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
	)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import qualified GHC.IO.FD


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Dominik
import Handler.Beamer
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

	-- Initialize the logging middleware
	logWare <- mkRequestLogger def
		{ outputFormat =
			if development
				then Detailed True
				else Apache FromSocket
		, destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
		}
	-- Create the WAI application and apply middlewares
	app <- toWaiAppPlain foundation
	return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
	manager <- newManager conduitManagerSettings
	s <- staticSite
	dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
			  Database.Persist.loadConfig >>=
			  Database.Persist.applyEnv
	p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)

	loggerSet' <- newLoggerSet defaultBufSize GHC.IO.FD.stdout
	(getter, _) <- clockDateCacher

	let logger = Yesod.Core.Types.Logger loggerSet' getter
	let foundation = App conf s p manager dbconf logger

	runLoggingT
		(Database.Persist.runPool dbconf (mapM_ runMigration [migrateServer]) p)
		(messageLoggerSource foundation logger)
	--Database.Persist.runPool dbconf (mapM_ runMigration [migrateCore, migrateVocabtrain, migrateVocabtrainServer]) p
--	return $ App conf s p manager dbconf --DominikSub
	return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
	defaultDevelApp loader makeApplication
  where
	loader = Yesod.Default.Config.loadConfig (configSettings Development)
		{ csParseExtra = parseExtra
		}
