{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}
module Handler.VocabtrainMobile where

import Database.Esqueleto (Value (..))
import Import
import BarefootSQL
import Codec.Compression.GZip
import CardType
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
import qualified Data.ByteString.Lazy as BSL
import Database.Persist.GenericSql.Raw (withStmt,execute)
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

import UserManipType
--import UserManipLog
import Data.Time (getCurrentTime) 

import Database.Persist.Store

data BasicAuthResult = BasicAuthAuthorized UserId | BasicAuthNothing | BasicAuthInvalidEncoding | BasicAuthWrongCreds

basicAuth :: GHandler App App BasicAuthResult
basicAuth = do 
	wr <- waiRequest
	case lookup "Authorization" $ Wai.requestHeaders wr of
		Nothing -> return BasicAuthNothing
		Just bstring -> do
			if BS.isPrefixOf basicHTTPAuthPrefix bstring
				then do
					$(logDebug) $ Text.concat [ "A", E.decodeUtf8 bstring, "A" ]
					case B64.decode $ BS.drop (BS.length basicHTTPAuthPrefix) bstring of
						Left _ -> return BasicAuthInvalidEncoding
						Right string -> do
							$(logDebug) $ Text.concat [ "A", E.decodeUtf8 string, "A" ]
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
--	let auth = BasicAuthAuthorized (Key $ PersistInt64 1)
	auth <- tokenAuth
	case auth of 
		BasicAuthAuthorized userId -> do
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
					time <- liftIO getCurrentTime
					$(logInfo) $ Text.concat [ "MobileDelta", Text.pack $ show userId, Text.pack $ show delta ]
					_ <- runDB $ do
						forM_ (deltaCardScript delta) $ \dcol -> do
								update (deltaCardColumnId dcol) [VocabCardScript =. (deltaCardColumnValue dcol)]
								insert $ VocabCardManip userId (deltaCardColumnId dcol) USERMANIP_UPDATE time $ deltaCardColumnValue dcol
						forM_ (deltaCardType delta) $ \dcol -> do
								update (deltaCardTypeColumnId dcol) [VocabCardType =. (deltaCardTypeColumnValue dcol)]
								insert $ VocabCardManip userId (deltaCardTypeColumnId dcol) USERMANIP_UPDATE time $ Text.pack $ show $ deltaCardTypeColumnValue dcol

						forM_ [(VocabCardScriptComment, deltaCardScriptComment), (VocabCardSpeech, deltaCardSpeech), (VocabCardSpeechComment, deltaCardSpeechComment)] $ 
							\(dbcons,deltacons) -> mapM_ (\dcol -> do
								update (deltaCardColumnId dcol) [dbcons =. (Just $ deltaCardColumnValue dcol)]
								insert $ VocabCardManip userId (deltaCardColumnId dcol) USERMANIP_UPDATE time $ deltaCardColumnValue dcol
								) $ deltacons delta

						forM_ (deltaTranslationContent delta) $ \dcol -> do
							mtranslation <- getBy $ UniqueTranslation (deltaTranslationColumnCardId dcol) (deltaTranslationColumnLanguage dcol)
							case mtranslation of 
								Just translation -> do
									let translationId = entityKey translation
									update translationId [VocabTranslationContent =. (deltaTranslationColumnValue dcol)]
									_ <- insert $ VocabTranslationManip userId translationId USERMANIP_UPDATE time $ deltaTranslationColumnValue dcol
									return ()
								Nothing -> return ()
						forM_ (deltaTranslationComment delta) $ \dcol -> do
							mtranslation <- getBy $ UniqueTranslation  (deltaTranslationColumnCardId dcol) (deltaTranslationColumnLanguage dcol)
							case mtranslation of 
								Just translation -> do
									let translationId = entityKey translation
									update translationId [VocabTranslationComment =. (Just $ deltaTranslationColumnValue dcol)]
									_ <- insert $ VocabTranslationManip userId translationId USERMANIP_UPDATE time $ deltaTranslationColumnValue dcol
									return ()
								Nothing -> return ()
					return $ RepPlain $ toContent successKeyword 
		_ -> permissionDenied ""
	where
		successKeyword :: Text
		successKeyword = "erfolgreich"

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
data DeltaCardTypeColumn = DeltaCardTypeColumn
	{ deltaCardTypeColumnId :: VocabCardId
	, deltaCardTypeColumnValue :: CardType
	}
	deriving Show
instance JS.FromJSON DeltaCardTypeColumn where
	parseJSON (JS.Object v) = DeltaCardTypeColumn <$>
		v .: "_id" <*>
		v .: "changes_value"
	parseJSON _ = mzero
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
	, deltaCardType :: [DeltaCardTypeColumn]
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
		v .: "changes_type" <*>
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

{-
instance JS.FromJSON BookSupply where
	parseJSON (JS.Object v) = BookSupply <$>
		v .: "_id" <*>
		v .: "book_name" <*>
		v .: "book_language" <*>
		v .: "book_timestamp" <*>
		v .: "book_translations"
	parseJSON _ = mzero
-}

instance JS.ToJSON TatoebaLanguage where
	toJSON = toJSON . show

instance JS.ToJSON (Entity VocabBook, [Entity VocabBookCache]) where
--	toJSON :: BookSupply -> JS.Value
	toJSON (ebook, cachedLanguages) = JS.object
		[ ("_id", JS.toJSON $ book_id)
		, ("book_name", JS.toJSON $ vocabBookName book)
		, ("book_language", JS.toJSON $ vocabBookLanguage book)
		, ("book_timestamp", JS.toJSON $ vocabBookTimestamp book)
		, ("book_translations", JS.toJSON $ map (vocabBookCacheBookLanguage . entityVal) cachedLanguages)
		]
			where 
				book_id = entityKey ebook
				book = entityVal ebook

postVocabtrainMobileBooksR :: Handler RepJson
postVocabtrainMobileBooksR = do
	bookSupply <- getBookSupply
--	languageSupply <- getTranslationLanguageSupply
	languageSupply <- runDB $ getVocabtrainTranslationLanguagesSQL
	userData <- getUserData
	$(logDebug) $ Text.pack $ show userData
	$(logDebug) $ Text.pack $ show $ (maybe [] (\t -> [("userdata", t)]) userData)
	jsonToRepJson $ JS.object ( [ ("books", JS.toJSON bookSupply), ("translation_languages", JS.toJSON $ map (\(Value a) -> a) languageSupply)] ++ (maybe [] (\t -> [("userdata", t)]) userData)   )
	where
		getBookSupply :: GHandler App App [(Entity VocabBook, [Entity VocabBookCache])]
		getBookSupply = do
			bookResults <- runDB $ selectList [] [Asc VocabBookName]
			forM bookResults $ \bookResult -> do
				let book_id  = entityKey bookResult
				cacheResults <- runDB $ selectList [VocabBookCacheBookId ==. book_id] []
				return $ (bookResult, cacheResults)
			{-
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
			-}
		{-
		getUserData = do
					timestampResult <- runDB $ C.runResourceT $ withStmt
						"SELECT max(filing_timestamp) FROM filing_data WHERE filing_user_id = 1;"
						[] C.$$ CL.consume
					return $ Just $ JS.object [("timestamp", JS.toJSON $ either (\_ -> 0::Int) Prelude.id $ fromPersistValue (timestampResult !! 0 !! 0) )]
		-}
		getUserData :: GHandler App App (Maybe JS.Value)
		getUserData = do
			--auth <- tokenAuth
			let auth = BasicAuthAuthorized (Key $ PersistInt64 1)
			case auth of 
				BasicAuthAuthorized userId -> do
--					timestampResult <- runDB $ getVocabtrainMaximumFilingTimestampOfUserSQL userId
					
					timestampResult <- runDB $ C.runResourceT $ withStmt
						"SELECT max(filing_timestamp) FROM filing_data WHERE filing_user_id = ?;"
						[unKey $ userId] C.$$ CL.consume
					return $ Just $ JS.object [("timestamp", JS.toJSON $ either (\_ -> 0::Int) Prelude.id $ fromPersistValue (timestampResult !! 0 !! 0) )]
					{-
					if null timestampResult 
						then return $ Just $ JS.object [("timestamp", JS.toJSON (0::Int) )]
						else
							let Value a = timestampResult !! 0
							in return $ Just $ JS.object [("timestamp", JS.toJSON a )]
					-}
				_ -> return Nothing

		

			{-
		getTranslationLanguageSupply :: GHandler App App [Text]
		getTranslationLanguageSupply = do
			languageResult <- runDB $ C.runResourceT $ withStmt
				"SELECT book_language from cache_book_translang GROUP BY book_language" 
				[] C.$$ CL.consume
			return $ map (\language -> either (\_ -> "") Prelude.id $ fromPersistValue (language !! 0)) languageResult
			-}

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

postVocabtrainMobileFilingDownloadR :: GHandler App App RepSqlite
postVocabtrainMobileFilingDownloadR = do
	auth <- tokenAuth
	case auth of 
		BasicAuthAuthorized userId -> do
			filingResult <- runDB $ selectList [ VocabFilingUserId ==. userId] []
			filingDataResult <- runDB $ selectList [ VocabFilingDataUserId ==. userId] []
			selectionResult <- runDB $ selectList [ VocabSelectionUserId ==. userId] []
			liftIO $ withTempFile "filingdownload" (\ file fileh  -> do
				C.runResourceT $ withSqliteConn (Text.pack file) $ \dbh -> do
					runSqlConn' dbh $ do
						runMigration migrateVocabtrainMobile
						forM_ filingResult     $ \row -> insert $ exportMobileFiling     $ entityVal row
						forM_ filingDataResult $ \row -> insert $ exportMobileFilingData $ entityVal row
						forM_ selectionResult  $ \row -> insert $ exportMobileSelection  $ entityVal row
				content <- liftIO $ BS.hGetContents fileh
				return $ RepSqlite $ toContent $ compress $ BSL.fromChunks [ content ]
				)	
			where
				runSqlConn' pers conn = runSqlConn conn pers
		_ -> permissionDenied ""

importMobileFiling :: UserId -> VocabMobileFiling -> VocabFiling
importMobileFiling userId mf = VocabFiling
	{ vocabFilingUserId = userId
	, vocabFilingCardId     = vocabMobileFilingCardId mf 
	, vocabFilingRank       = vocabMobileFilingRank mf
	, vocabFilingSession    = vocabMobileFilingSession mf
	, vocabFilingInterval   = vocabMobileFilingInterval mf
	, vocabFilingGrades     = vocabMobileFilingGrades mf
	, vocabFilingPriority   = vocabMobileFilingPriority mf
	, vocabFilingCount      = vocabMobileFilingCount mf
	, vocabFilingDifficulty = vocabMobileFilingDifficulty mf
	, vocabFilingSequence   = vocabMobileFilingSequence mf
	}

importMobileFilingData :: UserId -> VocabMobileFilingData -> VocabFilingData
importMobileFilingData userId mf = VocabFilingData
	{ vocabFilingDataUserId = userId
	, vocabFilingDataTimestamp = vocabMobileFilingDataTimestamp mf
	, vocabFilingDataSession   = vocabMobileFilingDataSession mf
	, vocabFilingDataSequence  = vocabMobileFilingDataSequence mf
	}

importMobileSelection :: UserId -> VocabMobileSelection -> VocabSelection
importMobileSelection userId mf = VocabSelection
	{ vocabSelectionUserId      = userId
	, vocabSelectionCardId      = vocabMobileSelectionCardId mf
	, vocabSelectionForgotten   = vocabMobileSelectionForgotten mf
	}

exportMobileFiling :: VocabFiling -> VocabMobileFiling
exportMobileFiling mf = VocabMobileFiling
	{ vocabMobileFilingCardId     = vocabFilingCardId mf 
	, vocabMobileFilingRank       = vocabFilingRank mf
	, vocabMobileFilingSession    = vocabFilingSession mf
	, vocabMobileFilingInterval   = vocabFilingInterval mf
	, vocabMobileFilingGrades     = vocabFilingGrades mf
	, vocabMobileFilingPriority   = vocabFilingPriority mf
	, vocabMobileFilingCount      = vocabFilingCount mf
	, vocabMobileFilingDifficulty = vocabFilingDifficulty mf
	, vocabMobileFilingSequence   = vocabFilingSequence mf
	}

exportMobileFilingData :: VocabFilingData -> VocabMobileFilingData
exportMobileFilingData mf = VocabMobileFilingData
	{ vocabMobileFilingDataTimestamp = vocabFilingDataTimestamp mf
	, vocabMobileFilingDataSession   = vocabFilingDataSession mf
	, vocabMobileFilingDataSequence  = vocabFilingDataSequence mf
	}

exportMobileSelection :: VocabSelection -> VocabMobileSelection
exportMobileSelection mf = VocabMobileSelection
	{ vocabMobileSelectionCardId      = vocabSelectionCardId mf
	, vocabMobileSelectionForgotten   = vocabSelectionForgotten mf
	}



postVocabtrainMobileFilingUploadR :: GHandler App App RepPlain
postVocabtrainMobileFilingUploadR = do
	auth <- tokenAuth
	case auth of 
		BasicAuthAuthorized userId -> do
			wr <- waiRequest
		--	bss <- liftIO $ mconcat <$> C.runResourceT (requestBody wr C.$= decompress defaultWindowBits C.$$ CL.consume)
		--	bss <- liftIO $ mconcat <$> C.runResourceT (requestBody wr C.$$ CL.consume)
			bss <- lift $ lazyConsume $ requestBody wr
			runInnerHandler <- handlerToIO
		--	let requestBodyString = BS.concat bss
			liftIO $ withTempFile "filing_upload" $ \file fileh  -> do
				BSL.hPut fileh $ decompress $ BSL.fromChunks bss
				C.runResourceT $ withSqliteConn (Text.pack file) $ \dbh -> do
--					$(logInfo) $ fromPersistValue $ unKey userId
{-					_ <- runSqlConn' dbh $ execute (Text.concat ["ALTER TABLE `filing` ADD COLUMN `filing_user_id` default '", either (\_ -> "0") Prelude.id $ fromPersistValue $ unKey userId, "';"]) []
					_ <- runSqlConn' dbh $ execute (Text.concat ["ALTER TABLE `filing_data` ADD COLUMN `filing_user_id` default '", either (\_ -> "0") Prelude.id $ fromPersistValue $ unKey userId, "';"]) []
					_ <- runSqlConn' dbh $ execute (Text.concat ["ALTER TABLE `selection` ADD COLUMN `selection_user_id` default '", either (\_ -> "0") Prelude.id $ fromPersistValue $ unKey userId, "';"]) [] -}
					$(logInfo) "a"
		--			runSqlConn' dbh $ updateWhere [] [VocabFilingUserId =. 1] -- TODO = 1
		--			filingList <- runSqlConn' dbh $ selectList [] [] 
					filingList <- runSqlConn' dbh $ selectList [] [] 
					filingDataList <- runSqlConn' dbh $ selectList [] []
					selectionList <- runSqlConn' dbh $ selectList [] []
					$(logInfo) "b"
		--			return (aa::[Entity VocabFiling])
					runInnerHandler $ do
						runDB $ do
							deleteWhere [VocabFilingUserId ==. userId]
							deleteWhere [VocabFilingDataUserId ==. userId]
							deleteWhere [VocabSelectionUserId ==. userId]
							mapM_ (\row -> insert $ importMobileFiling userId $ entityVal row) (filingList :: [Entity VocabMobileFiling])
							mapM_ (\row -> insert $ importMobileFilingData userId $ entityVal row) (filingDataList :: [Entity VocabMobileFilingData])
							mapM_ (\row -> insert $ importMobileSelection userId $ entityVal row) (selectionList :: [Entity VocabMobileSelection])
							execute "DELETE FROM filing WHERE filing_card_id IN ( SELECT filing_card_id FROM filing LEFT JOIN cards ON filing_card_id = cards._id WHERE cards._id IS NULL);" []
							execute "DELETE FROM selection WHERE selection_card_id IN ( SELECT selection_card_id FROM selection LEFT JOIN cards ON selection_card_id = cards._id WHERE cards._id IS NULL);" []
							--deleteWhere [VocabFilingUserId ==. userId]
							--deleteWhere [VocabFilingDataUserId ==. userId]
							--deleteWhere [VocabSelectionUserId ==. userId]
				return $ RepPlain $ toContent (""::Text)
			where
				runSqlConn' pers conn = runSqlConn conn pers
		_ -> permissionDenied ""

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
			-- probably some translations missing? Add random translations!
			translationMissingResult <- runDB $ rawSql
				(
					Text.concat 
						[ "select distinct on (translation_card_id) ?? from translations where translation_card_id in "
						, selectRawList cardResult
						, if null translationResult 
							then "" 
							else Text.concat
								[ " and _id not in "
								, selectRawList translationResult
								]
						, ";"]
				) [] :: GHandler App App [Entity VocabTranslation]
				--"SELECT _id, book_name, book_language, extract(epoch from book_timestamp) AS book_timestamp FROM books ORDER BY book_name ASC;"
			liftIO $ withTempFile "bookdownload" (\ file fileh  -> do
				C.runResourceT $ withSqliteConn (Text.pack file) $ \dbh -> do
					runSqlConn' dbh $ do
						_ <- runMigration migrateVocabtrainMobile
						forM_ bookResult               (\row -> insertKey (entityKey row) (entityVal row) )
						forM_ chapterResult            (\row -> insertKey (entityKey row) (entityVal row) )
						forM_ contentResult            (\row -> insertKey (entityKey row) (entityVal row) )
						forM_ cardResult               (\row -> insertKey (entityKey row) (entityVal row) )
						forM_ translationResult        (\row -> insertKey (entityKey row) (entityVal row) )
						forM_ translationMissingResult (\row -> insertKey (entityKey row) (entityVal row) )
				content <- liftIO $ BS.hGetContents fileh
				return $ RepSqlite $ toContent $ compress $ BSL.fromChunks [ content ]
				)	
			where
				runSqlConn' pers conn = runSqlConn conn pers

				selectRawList :: Show a => [Entity a] -> Text
				selectRawList l = Prelude.flip Text.snoc ')' $ Text.cons '(' $ Text.init . Text.tail . Text.pack . show $ 
					(map (\key -> either (\_ -> 0) Prelude.id $ fromPersistValue $ unKey $ entityKey key) l :: [Int] )

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
	tempdir <- Control.Exception.catch getTemporaryDirectory (\e -> do print $ show (e :: IOException);  return "/tmp")
	(tempfile, temph) <- openBinaryTempFile tempdir pattern 
	finally (func tempfile temph) (hClose temph >> removeFile tempfile)

