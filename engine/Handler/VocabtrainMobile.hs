{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric #-}
module Handler.VocabtrainMobile where

import Import
import Prelude (map)
import PostGenerated () 
import qualified Prelude
import Data.Aeson ((.:))
import qualified Data.Aeson as JS
import GHC.Generics

import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally, IOException)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Database.Persist.GenericSql.Raw (withStmt)
import Database.Persist.GenericSql
import Database.Persist.Sqlite

import Data.Conduit.Lazy
import qualified Data.Attoparsec.ByteString as JSP
import Network.Wai (requestBody)

import qualified Network.Wai as Wai
import qualified Data.Text.Encoding as E
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as B64

import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import System.IO
import Data.Word (Word8)
import qualified Data.List as List



data BasicAuthResult = BasicAuthAuthorized UserId | BasicAuthNothing | BasicAuthInvalidEncoding | BasicAuthWrongCreds

basicAuth :: GHandler App App BasicAuthResult
basicAuth = do 
	wr <- waiRequest
	case lookup "Authorization" $ Wai.requestHeaders wr of
		Nothing -> return BasicAuthNothing
		Just bstring -> do
			if BS.isPrefixOf basicHTTPAuthPrefix bstring
				then do
					$(logInfo) $ Text.concat [ "A", E.decodeUtf8 bstring, "A" ]
					case B64.decode $ BS.drop (BS.length basicHTTPAuthPrefix) bstring of
						Left _ -> return BasicAuthInvalidEncoding
						Right string -> do
							$(logInfo) $ Text.concat [ "A", E.decodeUtf8 string, "A" ]
							case Text.splitOn ":" $ E.decodeUtf8 string of
								[user, password] -> do
									userResult <- runDB $ selectList [ UserNick ==. Just user, UserPassword ==. Just password] []
									if length userResult == 0
										then return BasicAuthWrongCreds
										else return $ BasicAuthAuthorized (entityKey $ userResult!!0)
								_ -> return BasicAuthInvalidEncoding
				else return BasicAuthInvalidEncoding
	where
		basicHTTPAuthPrefix :: BS.ByteString
		basicHTTPAuthPrefix = "basic "

tokenAuth :: GHandler App App BasicAuthResult
tokenAuth = do 
	wr <- waiRequest
	case lookup "Authorization" $ Wai.requestHeaders wr of
		Nothing -> return BasicAuthNothing
		Just bstring -> do
			if BS.isPrefixOf tokenHTTPAuthPrefix bstring
				then do
					userResult <- runDB $ selectList [ UserAuthToken ==. (Just $ E.decodeUtf8 $ BS.drop (BS.length tokenHTTPAuthPrefix) bstring) ] []
					if length userResult == 0
						then return BasicAuthWrongCreds
						else return $ BasicAuthAuthorized (entityKey $ userResult!!0)
				else return BasicAuthInvalidEncoding
	where
		tokenHTTPAuthPrefix :: BS.ByteString
		tokenHTTPAuthPrefix = "token "


generateTokenString :: Gen (PrimState IO) -> IO BS.ByteString
generateTokenString g = do
    randomLen <- uniformR (20 :: Int, 40 :: Int) g
    str <- replicateM randomLen $ sequence $ Prelude.map (\t -> uniformR t g) [(c 'a', c 'z'), (c 'A', c 'Z'), (c '0', c '9')]
    return $ BS.pack $ List.concat str
	where
		c :: Char -> Word8
		c = fromIntegral . fromEnum

postVocabtrainMobileDeltaR :: GHandler App App RepPlain
postVocabtrainMobileDeltaR = do
	wr <- waiRequest
	bss <- lift $ lazyConsume $ requestBody wr
	let requestBodyString = BS.concat bss
	let mayBeDecoded = JSP.parse JS.json requestBodyString
	obtainParsed mayBeDecoded
	where
		obtainParsed :: JSP.IResult BS.ByteString JS.Value -> GHandler App App RepPlain
		obtainParsed (JSP.Fail _ errlist err) = invalidArgs ([Text.pack err] ++ map Text.pack errlist)
		obtainParsed (JSP.Partial cont) = obtainParsed $ (cont BS.empty) -- invalidArgs [ "Could only parse partial"::Text, continuation cont ]
		obtainParsed (JSP.Done _ res) = readRequest $ (JS.fromJSON res :: JS.Result VocabtrainDelta)

		readRequest :: JS.Result VocabtrainDelta -> GHandler App App RepPlain
		readRequest (JS.Error err) = invalidArgs [ Text.pack err ]
		readRequest (JS.Success delta) = do
		
			mapM_ (\dcol -> runDB $ update (deltaCardColumnId dcol) [VocabCardScript =. (deltaCardColumnValue dcol)]) $ deltaCardScript delta
			mapM_ (\(dbcons,deltacons) ->
				mapM_ (\dcol -> runDB $ update (deltaCardColumnId dcol) [dbcons =. (Just $ deltaCardColumnValue dcol)]) $ deltacons delta
				) [(VocabCardScriptComment, deltaCardScriptComment), (VocabCardSpeech, deltaCardSpeech), (VocabCardSpeechComment, deltaCardSpeechComment)]
			
			mapM_ (\dcol -> runDB $ updateWhere 
				[VocabTranslationCardId ==. (deltaTranslationColumnCardId dcol), VocabTranslationLanguage ==. (deltaTranslationColumnLanguage dcol)]
				[VocabTranslationContent =. (deltaTranslationColumnValue dcol)]) $
				deltaTranslationContent delta
			mapM_ (\dcol -> runDB $ updateWhere 
				[VocabTranslationCardId ==. (deltaTranslationColumnCardId dcol), VocabTranslationLanguage ==. (deltaTranslationColumnLanguage dcol)]
				[VocabTranslationComment =. (Just $ deltaTranslationColumnValue dcol)]) $
				deltaTranslationComment delta
		
			return $ RepPlain $ toContent ("erfolgreich"::Text)

getVocabtrainMobileAuthTokenR :: GHandler App App RepPlain
getVocabtrainMobileAuthTokenR = do 
	auth <- basicAuth
	case auth of 
		BasicAuthAuthorized userId -> do
			muser <- runDB $ get userId
			case muser of
				Just user ->
					case userAuthToken user of
						Just a -> return $ RepPlain $ toContent $ a
						Nothing -> do
							token <- liftIO $ withSystemRandom $ \gen -> generateTokenString gen
							runDB $ update userId [UserAuthToken =. (Just $ Text.concat [ E.decodeUtf8 token, "_", either (\_ -> ""::Text) Prelude.id $ fromPersistValue $ unKey userId ])]
							return $ RepPlain $ toContent $ token
				Nothing -> permissionDenied ""
		_ -> permissionDenied ""

data DeltaCardColumn = DeltaCardColumn
	{ deltaCardColumnId :: VocabCardId
	, deltaCardColumnValue :: Text
	}
	deriving Show
instance JS.FromJSON DeltaCardColumn where
	parseJSON (JS.Object v) = DeltaCardColumn <$>
		v .: "_id" <*>
		v .: "changes_value"
	parseJSON _ = mzero
data DeltaTranslationColumn = DeltaTranslationColumn
	{ deltaTranslationColumnCardId :: VocabCardId
	, deltaTranslationColumnValue :: Text
	, deltaTranslationColumnLanguage :: TatoebaLanguage
	}
	deriving Show
instance JS.FromJSON TatoebaLanguage where
	parseJSON = JS.withText "Text" (\t -> pure $ read $ Text.unpack t)
instance JS.FromJSON DeltaTranslationColumn where
	parseJSON (JS.Object v) = DeltaTranslationColumn <$>
		v .: "changes_card_id" <*>
		v .: "changes_value" <*>
		v .: "changes_language"
	parseJSON _ = mzero

data VocabtrainDelta = VocabtrainDelta
	{ deltaCardScript :: [DeltaCardColumn]
	, deltaCardScriptComment :: [DeltaCardColumn]
	, deltaCardSpeech :: [DeltaCardColumn]
	, deltaCardSpeechComment :: [DeltaCardColumn]
	, deltaTranslationContent :: [DeltaTranslationColumn]
	, deltaTranslationComment :: [DeltaTranslationColumn]
	}
	deriving Show
instance JS.FromJSON VocabtrainDelta where
	parseJSON (JS.Object v) = VocabtrainDelta <$>
		v .: "changes_script" <*>
		v .: "changes_script_comment" <*>
		v .: "changes_speech" <*>
		v .: "changes_speech_comment" <*>
		v .: "changes_vernicular" <*>
		v .: "changes_vernicular_comment"
	parseJSON _ = mzero

{-
data VocabtrainDelta = VocabtrainDelta
	{ deltaCardScript :: [Int]
	}
	deriving Show
instance JS.FromJSON VocabtrainDelta where
	parseJSON (JS.Object v) = VocabtrainDelta <$>
		v .: "changes"
	parseJSON _ = mzero
-}
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

postVocabtrainMobileBooksR :: Handler RepJson
postVocabtrainMobileBooksR = do
	bookSupply <- getBookSupply
	languageSupply <- getTranslationLanguageSupply
	userData <- getUserData
	$(logInfo) $ Text.pack $ show userData
	$(logInfo) $ Text.pack $ show $ (maybe [] (\t -> [("userdata", t)]) userData)
	jsonToRepJson $ JS.object ( [ ("books", JS.toJSON bookSupply), ("translation_languages", JS.toJSON languageSupply)] ++ (maybe [] (\t -> [("userdata", t)]) userData)   )
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
		getUserData :: GHandler App App (Maybe JS.Value)
		getUserData = do
			auth <- tokenAuth
			case auth of 
				BasicAuthAuthorized userId -> do
					muser <- runDB $ get userId
					case muser of
						Just user ->
							return $ Just $ JS.object [("timestamp", JS.toJSON (0::Int))]
						Nothing -> return Nothing
				_ -> return Nothing

		

		getTranslationLanguageSupply :: GHandler App App [Text]
		getTranslationLanguageSupply = do
			languageResult <- runDB $ C.runResourceT $ withStmt
				"SELECT book_language from cache_book_translang GROUP BY book_language" 
				[] C.$$ CL.consume
			return $ map (\language -> either (\_ -> "") Prelude.id $ fromPersistValue (language !! 0)) languageResult

data DownloadRequest = DownloadRequest 
	{ requestBooks :: [Int]
	, requestLanguages :: [Text] -- TODO [TatoebaLanguage]
	} deriving (Show, Generic)
instance JS.FromJSON DownloadRequest where
	parseJSON (JS.Object v) = DownloadRequest <$>
		v .: "books" <*>
		v .: "languages"
	parseJSON _ = mzero

{-	parseJSON (JS.Object v) = DownloadRequest <$>
		v .: "books" <*>
		v .: "languages"
	parseJSON _ = mzero
-}
typeSqlite :: ContentType
typeSqlite = "application/x-sqlite3"

newtype RepOctet = RepOctet Content
instance HasReps RepOctet where
    chooseRep (RepOctet c) _ = return (typeOctet, c)
newtype RepSqlite = RepSqlite Content
instance HasReps RepSqlite where
    chooseRep (RepSqlite c) _ = return (typeSqlite, c)

postVocabtrainMobileDownloadR :: GHandler App App RepSqlite
postVocabtrainMobileDownloadR = do
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
				VocabTranslationLanguage <-. ( map (read . Text.unpack) $ requestLanguages request)] []
			liftIO $ withTempFile "bookdownload" (\ file fileh  -> do
				C.runResourceT $ withSqliteConn (Text.pack file) (\dbh -> do
					_ <- ($) runSqlConn' dbh $ runMigrationSilent migrateAll
					forM_ bookResult (\row -> runSqlConn' dbh $ insertKey (entityKey row) (entityVal row) )
					forM_ chapterResult (\row -> runSqlConn' dbh $ insertKey (entityKey row) (entityVal row) )
					forM_ contentResult (\row -> runSqlConn' dbh $ insertKey (entityKey row) (entityVal row) )
					forM_ cardResult (\row -> runSqlConn' dbh $ insertKey (entityKey row) (entityVal row) )
					forM_ translationResult (\row -> runSqlConn' dbh $ insertKey (entityKey row) (entityVal row) )
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
