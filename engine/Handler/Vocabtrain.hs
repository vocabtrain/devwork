{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric #-}
module Handler.Vocabtrain where

import Import
import qualified Prelude
import qualified Data.Text as Text
import Data.Aeson ((.:))
import qualified Data.Aeson as JS
import GHC.Generics

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally, IOException)


import Control.Monad

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Database.Persist.GenericSql.Raw (withStmt)
import Database.Persist.GenericSql
import Database.Persist.Sqlite

import Data.Conduit.Lazy
import qualified Data.Attoparsec.ByteString as JSP
import Network.Wai (requestBody)

data BookSupply = BookSupply {
	bookSupplyId :: Int,
	bookSupplyName :: Text,
	bookSupplyLanguage :: Text,
	bookSupplyTimestamp :: Text,
	bookSupplyTranslatedLanguages :: [Text]
	}
	deriving Show

instance JS.FromJSON BookSupply where
	parseJSON (JS.Object v) = BookSupply <$>
		v .: "_id" <*>
		v .: "book_name" <*>
		v .: "book_language" <*>
		v .: "book_timestamp" <*>
		v .: "book_translations"
	parseJSON _ = mzero


instance JS.ToJSON BookSupply where
--	toJSON :: BookSupply -> JS.Value
	toJSON book = JS.object [
		("_id", JS.toJSON $ bookSupplyId book), 
		("book_name", JS.toJSON $ bookSupplyName book), 
		("book_language", JS.toJSON $ bookSupplyLanguage book), 
		("book_timestamp", JS.toJSON $ bookSupplyTimestamp book), 
		("book_translations", JS.toJSON $ bookSupplyTranslatedLanguages book)
		]

getVocabtrainBookSupplyR :: GHandler App App RepHtml
--getVocabtrainBookSupplyR = do
--displayBookSupply :: GHandler App App (
getVocabtrainBookSupplyR = do
	bookResult <- runDB $ selectList [] [] --VocabBookId ==. (Key $ toPersistValue (1::Int)) ] [] 
	chapterResults <- forM bookResult (\bookEntity -> runDB $ selectList [VocabChapterBookId ==. (entityKey $ bookEntity)] [])
	results <- return $ Prelude.zip bookResult chapterResults -- $ forM bookResult (\bookEntity -> return $ runDB $ selectList [VocabChapterId ==. (entityKey $ bookEntity)] []) 
	let a = either (\_ -> ""::Text) Prelude.id $ fromPersistValue $ unKey $ entityKey $ (bookResult!!0)

	defaultLayout $ toWidget $(whamletFile "templates/vocabtrain/booksupply.hamlet") 
{-
	defaultLayout [whamlet|
<ul>
$forall result <- results
    <li>#{vocabBookName $ entityVal $ fst result}
    <ol>
       $forall chapterEntity <- snd result
         <li>#{vocabChapterVolume $ entityVal chapterEntity}
|]
-}
{-
a = do
	let bookResult = [0..1]
	defaultLayout $ sequence $ forM bookResult (\bookEntity -> do
--		bookName <- vocabBookName $ entityVal bookEntity
		toWidget [whamlet|<a href=@{HomeR}>Go home!|]
		)
--	liftIO $ print $ map (vocabBookName . entityVal) bookResult
	--liftIO $ print (bookResult :: [Entity bookResult])
	--chapterResult <- runDB $ selectList [ VocabChapterBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
--	val <- VocabBookName $ entityVal $ (bookResult!!0)
--	defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]
-}	

{-getVocabtrainBookSupplyR :: Handler RepJson
getVocabtrainBookSupplyR = jsonToRepJson $ JS.toJSON ([5]::[Int])
-}

--postVocabtrainBookSupplyR :: Handler RepPlain
postVocabtrainBookSupplyR :: Handler RepJson
postVocabtrainBookSupplyR = do
	bookSupply <- getBookSupply
	languageSupply <- getTranslationLanguageSupply
	jsonToRepJson $ JS.object [ ("books", JS.toJSON bookSupply), ("translation_languages", JS.toJSON languageSupply)]
	where
		getBookSupply :: GHandler App App [BookSupply]
		getBookSupply = do
			bookResult <- runDB $ C.runResourceT $ withStmt
				"SELECT _id, book_name, book_language, extract(epoch from book_timestamp) AS book_timestamp FROM books ORDER BY book_name ASC;"
				[] C.$$ CL.consume
			sequence $ map (\book -> do
				languageResult <- runDB $ C.runResourceT $ withStmt
					"select book_language from cache_book_translang where book_id = ?" 
					[book !! 0] C.$$ CL.consume
				return $ BookSupply 
					(either (\_ -> -1) Prelude.id $ fromPersistValue (book !! 0)) 
					(either (\_ -> ""::Text) Prelude.id $ fromPersistValue (book !! 1)) 
					(either (\_ -> ""::Text) Prelude.id $ fromPersistValue (book !! 2)) 
					(either (\_ -> ""::Text) Prelude.id $ fromPersistValue (book !! 3)) 
					(map (\language -> either (\_ -> ""::Text) Prelude.id $ fromPersistValue (language !! 0)) languageResult)
				) bookResult

		getTranslationLanguageSupply :: GHandler App App [Text]
		getTranslationLanguageSupply = do
			languageResult <- runDB $ C.runResourceT $ withStmt
				"SELECT book_language from cache_book_translang GROUP BY book_language" 
				[] C.$$ CL.consume
			return $ map (\language -> either (\_ -> "") Prelude.id $ fromPersistValue (language !! 0)) languageResult

data DownloadRequest = DownloadRequest 
	{ requestBooks :: [Int]
	, requestLanguages :: [Text]
	} deriving (Show, Generic)
instance JS.FromJSON DownloadRequest where

typeSqlite :: ContentType
typeSqlite = "application/x-sqlite3"

newtype RepOctet = RepOctet Content
instance HasReps RepOctet where
    chooseRep (RepOctet c) _ = return (typeOctet, c)
newtype RepSqlite = RepSqlite Content
instance HasReps RepSqlite where
    chooseRep (RepSqlite c) _ = return (typeSqlite, c)

postVocabtrainDownloadR :: GHandler App App RepSqlite
postVocabtrainDownloadR = do
	wr <- waiRequest
	bss <- lift $ lazyConsume $ requestBody wr
	let requestBodyString = BS.concat bss
	let mayBeDecoded = JSP.parse JS.json requestBodyString
	obtainParsed mayBeDecoded
	where
		obtainParsed :: JSP.IResult t JS.Value -> GHandler App App RepSqlite
		obtainParsed (JSP.Fail _ _ err) = invalidArgs [Text.pack err]
		obtainParsed (JSP.Partial _) = invalidArgs [ "Could only parse partial"::Text ]
		obtainParsed (JSP.Done _ res) = readRequest $ (JS.fromJSON res :: JS.Result DownloadRequest)

		readRequest :: JS.Result DownloadRequest -> GHandler App App RepSqlite
		readRequest (JS.Error err) = invalidArgs [ Text.pack err ]
		readRequest (JS.Success request) = do
			bookResult <- runDB $ selectList [ VocabBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
			chapterResult <- runDB $ selectList [ VocabChapterBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
			contentResult <- runDB $ selectList [ VocabContentChapterId <-. ( map entityKey chapterResult)] []
			cardResult <- runDB $ selectList [ VocabCardId <-. ( map (vocabContentCardId . entityVal) contentResult)] []
			translationResult <- runDB $ selectList 
				[ VocabTranslationCardId <-. ( map entityKey cardResult), 
				VocabTranslationLanguage <-. (requestLanguages request)] []
			liftIO $ withTempFile "bookdownload" (\ file fileh  -> do
				withSqliteConn (Text.pack file) (\dbh -> do
					_ <- ($) runSqlConn' dbh $ runMigrationSilent migrateAll
					forM_ bookResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
					forM_ chapterResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
					forM_ contentResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
					forM_ cardResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
					forM_ translationResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
					)
				content <- liftIO $ BS.hGetContents fileh
				return $ RepSqlite $ toContent content
				)	
			where
				runSqlConn' pers conn = runSqlConn conn pers

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
	tempdir <- Control.Exception.catch getTemporaryDirectory (\e -> do print $ show (e :: IOException);  return "/tmp")
	(tempfile, temph) <- openBinaryTempFile tempdir pattern 
	finally (func tempfile temph) (hClose temph >> removeFile tempfile)


{-
 -

postVocabtrainDownloadR' :: GHandler App App RepPlain
postVocabtrainDownloadR' = do
	wr <- waiRequest
	bss <- lift $ lazyConsume $ requestBody wr
	let requestBodyString = BS.concat bss
	let mayBeDecoded = JSP.parse JS.json requestBodyString
	obtainParsed mayBeDecoded

obtainParsed :: JSP.IResult t JS.Value -> GHandler App App RepPlain
obtainParsed (JSP.Fail _ _ err) = return $ RepPlain $ toContent $ "E: " ++ err
obtainParsed (JSP.Partial _) = return $ RepPlain $ toContent ("Could only parse partial"::Text)
obtainParsed (JSP.Done _ res) = readRequest $ (JS.fromJSON res :: JS.Result DownloadRequest)

readRequest :: JS.Result DownloadRequest -> GHandler App App RepPlain
readRequest (JS.Error err) = return $ RepPlain $ toContent err
readRequest (JS.Success request) = do
	bookResult <- runDB $ selectList [ VocabBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
	chapterResult <- runDB $ selectList [ VocabChapterBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
	contentResult <- runDB $ selectList [ VocabContentChapterId <-. ( map entityKey chapterResult)] []
	cardResult <- runDB $ selectList [ VocabCardId <-. ( map (vocabContentCardId . entityVal) contentResult)] []
	translationResult <- runDB $ selectList [ VocabTranslationCardId <-. ( map entityKey cardResult)] []
	withSqliteConn "test.db" (\dbh -> do
		_ <- ($) runSqlConn' dbh $ runMigrationSilent migrateAll
		forM_ bookResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
		forM_ chapterResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
		forM_ contentResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
		forM_ cardResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
		forM_ translationResult (\row -> runSqlConn' dbh $ insert $ entityVal row)
		)

-}

--	putResult bookResult
--	putResult chapterResult
--	putResult translationResult
--	forM bookResult (\bookEntity -> runSqlConn' dbh $ insert $ entityVal bookEntity)
--	forM chapterResult (\chapterEntity -> runSqlConn' dbh $ insert $ entityVal chapterEntity)
{-
	liftIO $ print (bookResult :: [Entity VocabBook])
	forM bookResult (\bookEntity -> do
		runSqlConn' dbh $ insert $ entityVal bookEntity
		forM chapterResult (\chapterEntity -> do
			let a = entityKey chapterEntity
			runSqlConn' dbh $ insert $ entityVal chapterEntity
			
			)
		)
	return $ RepPlain $ toContent ("a"::Text)
	where
--		runSqlConn' :: MonadCatchIO m => Connection -> SqlPersist m a -> m a
		runSqlConn' pers conn = runSqlConn conn pers

-}
{-
readRequest :: JS.Result DownloadRequest -> GHandler App App RepPlain
readRequest (JS.Error err) = return $ RepPlain $ toContent err
readRequest (JS.Success request) = do
	bookResult <- runDB $ selectList [ BookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
	liftIO $ print (bookResult :: [Entity Book])
	withSqliteConn "test.db" $ runSqlConn $ do
		runMigrationSilent migrateAll
		forM bookResult (\bookEntity -> insert $ entityVal bookEntity)
	forM bookResult (\bookEntity -> do
		chapterResult <- runDB $ selectList [ ChapterBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
		withSqliteConn "test.db" $ runSqlConn $ do
			forM chapterResult (\chapterEntity -> insert $ entityVal chapterEntity)
		)
	
	return $ RepPlain $ toContent ("a"::Text)
-}

				{-
					conn <- liftIO $ connectSqlite3 "test1.db"
					runRaw dbh createSqliteTables
					commit dbh
					bookResult <- runDB $ C.runResourceT $ withStmt
						("SELECT _id, book_name, book_language, extract(epoch from book_timestamp) AS book_timestamp FROM books WHERE book_id IN ("
							++ (List.intersperse "," (requestBooks request)) ++ ")"
						)
						[] C.$$ CL.consume
					bookStmt <- prepare dbh "INSERT INTO books (_id, book_name, book_language, book_timestamp) VALUES (:book_id, :book_name, :book_language, :book_timestamp)"
					executeMany bookStmt bookResult

					dbh <- liftIO $ connectPostgreSQL "host=localhost dbname=vocabtrain user=postgres"
					bookResult <- quickQuery' dbh "SELECT _id, book_name, book_language, extract(epoch from book_timestamp) AS book_timestamp FROM books WHERE book_id IN " []
-}

--		show $ ((JS.fromJSON res) :: JS.Result DownloadRequest)
--			where
				
		{-do



createSqliteTables :: String
createSqliteTables = [literal|
CREATE TABLE IF NOT EXISTS `books` (
`_id` INTEGER,
`book_name` TEXT NOT NULL,
`book_language` TEXT DEFAULT \'ja\',
`book_timestamp` INTEGER DEFAULT \'0\',
PRIMARY KEY(`_id`),
UNIQUE ( `book_name`)
);
CREATE TABLE IF NOT EXISTS `chapters` (
`_id` INTEGER,
`chapter_book_id` INTEGER NOT NULL,
`chapter_volume` TEXT NOT NULL,
PRIMARY KEY(`_id`),
UNIQUE ( `chapter_book_id`, `chapter_volume` )
);
CREATE TABLE IF NOT EXISTS `content` (
`_id` INTEGER  ,
`content_chapter_id` INTEGER NOT NULL,
`content_card_id` INTEGER NOT NULL,
PRIMARY KEY(`_id`),
UNIQUE( `content_chapter_id`, `content_card_id`)
);
CREATE TABLE IF NOT EXISTS `cards` (
`_id` INTEGER  ,
`card_kana` TEXT NOT NULL ,
`card_kana_comment` TEXT ,
`card_kanji` TEXT,
`card_kanji_comment` TEXT ,
`card_type` INT,
PRIMARY KEY(`_id`),
UNIQUE ( `card_kanji`, `card_kana`, `card_kana_comment`, `card_kanji_comment`)
);
CREATE TABLE IF NOT EXISTS `translations` (
`_id` INTEGER  ,
`translation_card_id` INTEGER NOT NULL,
`translation_language` TEXT NOT NULL,
`translation_content` TEXT NOT NULL ,
`translation_comment` TEXT ,
PRIMARY KEY(`_id`),
UNIQUE ( `translation_card_id`, `translation_language`)
);
CREATE TABLE IF NOT EXISTS `android_metadata` (`locale` TEXT DEFAULT \'en_US\');
INSERT INTO "android_metadata" VALUES(\'en_US\');
|]

-}
