{-# LANGUAGE OverloadedStrings, ConstraintKinds  #-}
module BarefootSQL where

import Database.Esqueleto
import qualified Database.Persist.Query as OldQuery
import Model
--import Prelude (map, ($))
import Prelude 
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Int
import Database.Esqueleto.Internal.Sql
import Generated
import PostGenerated

import Import (runDB)

maxSQL :: UnsafeSqlFunctionArgument a => a -> SqlExpr (Value b)
maxSQL = unsafeSqlFunction "MAX"

isSQL :: SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
isSQL = unsafeSqlBinOp " IS "

isVocabtrainCardAlreadyInChapterSQL :: (MonadLogger m, MonadResourceBase m) => VocabCardId -> SqlPersist m [Value Int]
isVocabtrainCardAlreadyInChapterSQL cardId =
	select $ from $ \(content,chapter) -> do
	where_ (content ^. VocabContentChapterId ==. chapter ^. VocabChapterId)
	where_ (content ^. VocabContentCardId ==. val cardId)
	return countRows

-- SELECT ?? FROM cards WHERE NOT EXISTS ( SELECT 1 FROM content WHERE content_card_id = cards._id);"
getVocabtrainOrphanedCardsSQL :: (MonadLogger m, MonadResourceBase m) => SqlPersist m [Entity VocabCard]
getVocabtrainOrphanedCardsSQL =
	select $ from $ \card -> do
	where_ $ notExists $ 
		from $ \content -> do
		where_ ( content ^. VocabContentCardId ==. card  ^. VocabCardId)
	return card

-- "SELECT book_language from cache_book_translang GROUP BY book_language" 
getVocabtrainTranslationLanguagesSQL :: (MonadLogger m, MonadResourceBase m) => SqlPersist m [Value TatoebaLanguage]
getVocabtrainTranslationLanguagesSQL = 
	select $ from $ \l -> do
	groupBy (l ^. VocabBookCacheBookLanguage)
	return (l ^. VocabBookCacheBookLanguage)

{-
-- "SELECT max(filing_timestamp) FROM filing_data WHERE filing_user_id = ?;"
getVocabtrainMaximumFilingTimestampOfUserSQL :: (MonadLogger m, MonadResourceBase m) => UserId -> SqlPersist m [Value Int64]
getVocabtrainMaximumFilingTimestampOfUserSQL userId =
	select $ from $ \filing -> do
	where_ (filing ^. VocabFilingDataUserId ==. val userId)
	return $ maxSQL $ filing ^. VocabFilingDataTimestamp
-}	

-- "SELECT ?? FROM cards JOIN content ON content_card_id = cards._id JOIN chapters ON content_chapter_id = chapters._id WHERE chapters._id = ?;"
getVocabtrainCardsOfChapterSQL :: (MonadLogger m, MonadResourceBase m) => VocabChapterId -> SqlPersist m [Entity VocabCard]
getVocabtrainCardsOfChapterSQL chapterId =
	select $ from $ \(card,content) -> do
	where_ ( card ^. VocabCardId ==. content ^. VocabContentCardId)
	where_ (content ^. VocabContentChapterId ==. val chapterId)
	return card
-- select * from books join chapters on books._id = chapter_book_id join content on content_chapter_id = chapters._id join cards on content_card_id = cards._id left join translations on translation_card_id = cards._id and translation_language = 'deu' where books._id = 3 and translation_language is null;
getVocabtrainTranslationsMissingForBook :: (MonadLogger m, MonadResourceBase m) => VocabBookId -> TatoebaLanguage -> SqlPersist m [Entity VocabCard]
getVocabtrainTranslationsMissingForBook bookId language =
	select $ from $ \( chapter `InnerJoin` content `InnerJoin` card `LeftOuterJoin` translation) -> do
	where_ (chapter ^. VocabChapterBookId ==. val bookId)
	on $ (translation ^. VocabTranslationCardId ==. card ^. VocabCardId) &&. (translation ^. VocabTranslationLanguage ==. val language)
	on $ card ^. VocabCardId ==. content ^. VocabContentCardId
	on $ chapter ^. VocabChapterId ==. content ^. VocabContentChapterId
	where_ $ translation ^. VocabTranslationLanguage `isSQL` nothing
	return card

-- select * from cards where cards._id NOT IN (SELECT translation_card_id FROM translations);
deleteVocabtrainNotTranslatedCards :: (MonadLogger m, MonadResourceBase m) => SqlPersist m ()
deleteVocabtrainNotTranslatedCards = 
	let subquery = from $ \translation -> return $ translation ^. VocabTranslationCardId
	in delete $ from $ \card -> do
		where_ $ card ^. VocabCardId `notIn` subList_select subquery

getVocabtrainNotTranslatedCards :: (MonadLogger m, MonadResourceBase m) => SqlPersist m [Entity VocabCard]
getVocabtrainNotTranslatedCards = 
	let subquery = from $ \translation -> return $ translation ^. VocabTranslationCardId
	in select $ from $ \card -> do
		where_ $ card ^. VocabCardId `notIn` subList_select subquery
		return card
