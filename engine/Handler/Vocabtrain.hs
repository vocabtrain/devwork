{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric #-}
module Handler.Vocabtrain where

import Import
import qualified Prelude
import Data.Aeson ((.:))
import qualified Data.Aeson as JS
import GHC.Generics


import Control.Monad

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.GenericSql.Raw (withStmt)


data BookSupply = BookSupply {
	bookId :: Int,
	bookName :: Text,
	bookLanguage :: Text,
	bookTimestamp :: Text,
	bookTranslatedLanguages :: [Text]
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
		("_id", JS.toJSON $ bookId book), 
		("book_name", JS.toJSON $ bookName book), 
		("book_language", JS.toJSON $ bookLanguage book), 
		("book_timestamp", JS.toJSON $ bookTimestamp book), 
		("book_translations", JS.toJSON $ bookTranslatedLanguages book)
		]



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

data DownloadRequest = DownloadRequest {
	requestBooks :: [String],
	requestLanguages :: [String]
	} deriving (Show, Generic)
instance JS.FromJSON DownloadRequest where


{-
postVocabtrainDownloadR = do
	wr <- waiRequest
	bss <- lift $ lazyConsume $ requestBody wr
	let requestBody = BS.concat bss
	--let mayBeDecoded = JS.decode requestBody :: Maybe DownloadRequest
	let mayBeDecoded = JSP.parse JS.json requestBody
--	liftIO $ print mayBeDecoded
	liftIO $ print $ obtainParsed mayBeDecoded
	return $ RepPlain $ toContent $ ("Error" :: Text)
	where
--		returnParsed (Just decoded) = return $ RepPlain $ toContent $ show decoded
--		returnParsed Nothing = return $ RepPlain $ toContent $ ("Error" :: Text)
		obtainParsed (JSP.Fail t errs err) =  err
		obtainParsed (JSP.Done t res) = readRequest $ (JS.fromJSON res :: JS.Result DownloadRequest)
			where
				readRequest (JS.Error err) = err
				readRequest (JS.Success request) = do
					dbh <- liftIO $ connectPostgreSQL "host=localhost dbname=vocabtrain user=postgres"
					bookResult <- quickQuery' dbh "SELECT _id, book_name, book_language, extract(epoch from book_timestamp) AS book_timestamp FROM books WHERE book_id IN " []
-}

--		show $ ((JS.fromJSON res) :: JS.Result DownloadRequest)
--			where
				
		{-do
-}


