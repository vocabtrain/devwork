{-# LANGUAGE OverloadedStrings, ConstraintKinds  #-}
module BarefootSQL where

import Database.Esqueleto
--import qualified Database.Persist.Query as OldQuery
import Model
--import Prelude (map, ($))
import Prelude 
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Esqueleto.Internal.Sql
import Generated
-- import PostGenerated
import Data.Maybe (listToMaybe)

maxSQL :: UnsafeSqlFunctionArgument a => a -> SqlExpr (Value b)
maxSQL = unsafeSqlFunction "MAX"

isSQL :: SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
isSQL = unsafeSqlBinOp " IS "


-- "SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences JOIN tatoeba_links ON sentence_id = link_translation_id WHERE link_sentence_id = IN(?...?)"
getTatoebaTranslationsSQLP :: (MonadLogger m, MonadResourceBase m) => [TatoebaSentenceId] -> SqlPersistT m [(Entity TatoebaSentence, Entity TatoebaLink)]
getTatoebaTranslationsSQLP sentence_ids =
	select $ from $ \(sentence `InnerJoin` link) -> do
	on $ sentence ^. TatoebaSentenceId ==. link ^. TatoebaLinkTranslationId
	where_ $ link ^. TatoebaLinkSentenceId `in_` (valList sentence_ids)
	return (sentence, link)


-- "SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences JOIN tatoeba_links ON sentence_id = link_translation_id WHERE link_sentence_id = ?"
getTatoebaTranslationsSQL :: (MonadLogger m, MonadResourceBase m) => TatoebaSentenceId -> SqlPersistT m [Entity TatoebaSentence]
getTatoebaTranslationsSQL sentence_id =
	select $ from $ \(sentence `InnerJoin` link) -> do
	on $ sentence ^. TatoebaSentenceId ==. link ^. TatoebaLinkTranslationId
	where_ $ link ^. TatoebaLinkSentenceId ==. val sentence_id
	return sentence

-- "SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences WHERE sentence_language = ? ORDER BY RANDOM() LIMIT 1;"
getTatoebaRandomSentenceWithLanguageSQL :: (MonadLogger m, MonadResourceBase m) => TatoebaLanguage -> SqlPersistT m (Maybe (Entity TatoebaSentence))
getTatoebaRandomSentenceWithLanguageSQL language = (return . listToMaybe) =<<  do
	select $ from $ \sentence -> do
	where_ $ sentence ^. TatoebaSentenceLanguage ==. val language
	orderBy [asc (random_ :: SqlExpr (Value Double)) ]
	limit 1
	return sentence

-- "SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences ORDER BY RANDOM() LIMIT 1;" 
getTatoebaRandomSentenceSQL :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m (Maybe (Entity TatoebaSentence))
getTatoebaRandomSentenceSQL = (return . listToMaybe) =<<  do
	select $ from $ \sentence -> do
	orderBy [asc (random_ :: SqlExpr (Value Double)) ]
	limit 1
	return $ sentence

-- "SELECT sentence_language from tatoeba_sentences group by sentence_language;"
getTatoebaLanguagesSQL :: (MonadLogger m, MonadResourceBase m) =>  SqlPersistT m [Value TatoebaLanguage]
getTatoebaLanguagesSQL =
	select $ from $ \sentence -> do
	groupBy $ sentence ^. TatoebaSentenceLanguage
	return $ sentence ^. TatoebaSentenceLanguage

-- "DELETE FROM filing WHERE filing_card_id IN ( SELECT filing_card_id FROM filing LEFT JOIN cards ON filing_card_id = cards._id WHERE cards._id IS NULL);"
deleteVocabtrainFilingWhereCardMissing :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m ()
deleteVocabtrainFilingWhereCardMissing =
	let subquery = from $ \(filing `LeftOuterJoin` card) -> do
		on $ filing ^. VocabFilingCardId ==. card ^. VocabCardId
		where_ $ card ^. VocabCardId `isSQL` nothing
		return $ filing ^. VocabFilingCardId
	in delete $ from $ \filing -> where_ $ filing ^. VocabFilingCardId `in_` subList_select subquery


-- "DELETE FROM selection WHERE selection_card_id IN ( SELECT selection_card_id FROM selection LEFT JOIN cards ON selection_card_id = cards._id WHERE cards._id IS NULL);"
deleteVocabtrainSelectionWhereCardMissing :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m ()
deleteVocabtrainSelectionWhereCardMissing =
	let subquery = from $ \(selection `LeftOuterJoin` card) -> do
		on $ selection ^. VocabSelectionCardId ==. card ^. VocabCardId
		where_ $ card ^. VocabCardId `isSQL` nothing
		return $ selection ^. VocabSelectionCardId
	in delete $ from $ \selection -> where_ $ selection ^. VocabSelectionCardId `in_` subList_select subquery

-- select ?? from chapters join content on content_chapter_id = chapters._id where content_card_id = 2;
getVocabtrainChaptersWithContainingCardSQL :: (MonadLogger m, MonadResourceBase m) => VocabCardId -> SqlPersistT m [Entity VocabChapter]
getVocabtrainChaptersWithContainingCardSQL cardId =
	select $ from $ \(chapter `InnerJoin` content) -> do
	where_ $ content ^. VocabContentCardId ==. val cardId
	on $ chapter ^. VocabChapterId ==. content ^. VocabContentChapterId
	orderBy [asc (chapter ^. VocabChapterVolume)]
	return chapter


isVocabtrainCardAlreadyInChapterSQL :: (MonadLogger m, MonadResourceBase m) => VocabCardId -> SqlPersistT m [Value Int]
isVocabtrainCardAlreadyInChapterSQL cardId =
	select $ from $ \(content,chapter) -> do
	where_ (content ^. VocabContentChapterId ==. chapter ^. VocabChapterId)
	where_ (content ^. VocabContentCardId ==. val cardId)
	return countRows

-- SELECT ?? FROM cards WHERE NOT EXISTS ( SELECT 1 FROM content WHERE content_card_id = cards._id);"
getVocabtrainOrphanedCardsSQL :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m [Entity VocabCard]
getVocabtrainOrphanedCardsSQL =
	select $ from $ \card -> do
	where_ $ notExists $ 
		from $ \content -> do
		where_ ( content ^. VocabContentCardId ==. card  ^. VocabCardId)
	return card

-- "SELECT book_language from cache_book_translang GROUP BY book_language" 
getVocabtrainTranslationLanguagesSQL :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m [Value TatoebaLanguage]
getVocabtrainTranslationLanguagesSQL = 
	select $ from $ \l -> do
	groupBy (l ^. VocabBookCacheBookLanguage)
	return (l ^. VocabBookCacheBookLanguage)


-- "SELECT max(filing_timestamp) FROM filing_data WHERE filing_user_id = ?;"
getVocabtrainMaximumFilingTimestampOfUserSQL :: (MonadLogger m, MonadResourceBase m) => UserId -> SqlPersistT m [Value (Maybe Int)]
getVocabtrainMaximumFilingTimestampOfUserSQL userId =
	select $ from $ \filing -> do
	where_ (filing ^. VocabFilingDataUserId ==. val userId)
	return $ max_ $ filing ^. VocabFilingDataTimestamp -- TODO: Does this work?
	

-- "SELECT ?? FROM cards JOIN content ON content_card_id = cards._id JOIN chapters ON content_chapter_id = chapters._id WHERE chapters._id = ?;"
getVocabtrainCardsOfChapterSQL :: (MonadLogger m, MonadResourceBase m) => VocabChapterId -> SqlPersistT m [Entity VocabCard]
getVocabtrainCardsOfChapterSQL chapterId =
	select $ from $ \(card,content) -> do
	where_ ( card ^. VocabCardId ==. content ^. VocabContentCardId)
	where_ (content ^. VocabContentChapterId ==. val chapterId)
	return card
-- select * from books join chapters on books._id = chapter_book_id join content on content_chapter_id = chapters._id join cards on content_card_id = cards._id left join translations on translation_card_id = cards._id and translation_language = 'deu' where books._id = 3 and translation_language is null;
getVocabtrainTranslationsMissingForBook :: (MonadLogger m, MonadResourceBase m) => VocabBookId -> TatoebaLanguage -> SqlPersistT m [Entity VocabCard]
getVocabtrainTranslationsMissingForBook bookId language =
	select $ from $ \( chapter `InnerJoin` content `InnerJoin` card `LeftOuterJoin` translation) -> do
	where_ (chapter ^. VocabChapterBookId ==. val bookId)
	on $ (translation ^. VocabTranslationCardId ==. card ^. VocabCardId) &&. (translation ^. VocabTranslationLanguage ==. val language)
	on $ card ^. VocabCardId ==. content ^. VocabContentCardId
	on $ chapter ^. VocabChapterId ==. content ^. VocabContentChapterId
	where_ $ translation ^. VocabTranslationLanguage `isSQL` nothing
	return card

-- select * from cards where cards._id NOT IN (SELECT translation_card_id FROM translations);
deleteVocabtrainNotTranslatedCards :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m ()
deleteVocabtrainNotTranslatedCards = 
	let subquery = from $ \translation -> return $ translation ^. VocabTranslationCardId
	in delete $ from $ \card -> do
		where_ $ card ^. VocabCardId `notIn` subList_select subquery

getVocabtrainNotTranslatedCards :: (MonadLogger m, MonadResourceBase m) => SqlPersistT m [Entity VocabCard]
getVocabtrainNotTranslatedCards = 
	let subquery = from $ \translation -> return $ translation ^. VocabTranslationCardId
	in select $ from $ \card -> do
		where_ $ card ^. VocabCardId `notIn` subList_select subquery
		return card

