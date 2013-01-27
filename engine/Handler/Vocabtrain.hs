{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}
module Handler.Vocabtrain where

import Import
import Sphinx
import ToAppMessage 
import PostGenerated () 
import CardType
import UserManipType
import UserManipLog
import qualified Prelude
import qualified Data.Text as Text
import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe
import Control.Monad.Maybe

--import Database.Persist.Query.Join.Sql (runJoin)
import Database.Persist.Query.Join (SelectOneMany (..), selectOneMany, runJoin)

import Control.Monad

import Database.Persist.GenericSql.Raw (SqlBackend)
import Database.Persist.GenericSql
import Database.Persist.Store

import Data.Time (getCurrentTime) 

import Control.Arrow

import qualified Data.Aeson as JS

vocabLayoutSheet :: GWidget App App () -> SheetLayout App (Route App)
vocabLayoutSheet widget = SheetLayout { 
	  sheetTitle = "Vocabtrain"
	, sheetNav = Nothing
	, sheetBanner = Just $(widgetFile "vocabtrain/banner") 
	, sheetContent = widget
	}

vocabLayout :: GWidget App App () -> GHandler App App RepHtml
vocabLayout widget = globalLayout $ vocabLayoutSheet widget

{-
-- Book Form
-}

bookForm :: Maybe VocabBook -> AForm App App VocabBook
bookForm mbook = do
	VocabBook
		<$> areq textField (fieldSettingsLabel MsgFieldName) (vocabBookName <$> mbook)
		<*> areq (selectField getTatoebaLanguageOptionList) (fieldSettingsLabel MsgFieldLanguage) (vocabBookLanguage <$> mbook)
		<*> aformM (liftIO getCurrentTime)

getTatoebaLanguageOptionList :: GHandler App App (OptionList TatoebaLanguage)
getTatoebaLanguageOptionList = do
	msgShow <- getMessageRender
	optionsPairs $ List.sortBy (Ord.comparing fst) $
		Prelude.map (msgShow . toAppMessage &&& Prelude.id) $ [(minBound::TatoebaLanguage)..(maxBound::TatoebaLanguage)]

widgetVocabtrainBookCreate :: GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainBookCreate bookFormWidget bookFormEnctype = toWidget $(whamletFile "templates/vocabtrain/book_create.hamlet") 

getVocabtrainR :: GHandler App App RepHtml
getVocabtrainR = do
	setUltDestCurrent
	maid <- maybeAuth
	bookResult <- runDB $ selectList [] [Asc VocabBookName] --VocabBookId ==. (Key $ toPersistValue (1::Int)) ] [] 
	chapterResults <- forM bookResult (\bookEntity -> runDB $ selectList [VocabChapterBookId ==. (entityKey $ bookEntity)] [Asc VocabChapterVolume])
	results <- return $ Prelude.zip bookResult chapterResults -- $ forM bookResult (\bookEntity -> return $ runDB $ selectList [VocabChapterId ==. (entityKey $ bookEntity)] []) 
	let a = either (\_ -> ""::Text) Prelude.id $ fromPersistValue $ unKey $ entityKey $ (bookResult!!0)
	(bookFormWidget, bookFormEnctype) <- generateFormPost $ renderDivs $ bookForm Nothing

	widgetBook <- widgetToPageContent $ widgetVocabtrainBookCreate bookFormWidget bookFormEnctype
	let vocabBookLayout widget = globalLayout $ (vocabLayoutSheet widget) { 
				 sheetNav = Just $(whamletFile "templates/vocabtrain/navbook.hamlet")
				}
	vocabBookLayout $ toWidget $(whamletFile "templates/vocabtrain/books.hamlet") 



postVocabtrainBookR :: GHandler App App RepHtml
postVocabtrainBookR = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, bookFormWidget), bookFormEnctype) <- runFormPost $ renderDivs $ bookForm Nothing
			case result of
				FormSuccess book -> do -- vocabLayout [whamlet|<p>#{show book}|]
					bookId <- runDB $ insert book
					_ <- runDB $ insert $ VocabBookManip (entityKey aid) bookId USERMANIP_INSERT (vocabBookTimestamp book) $ vocabBookName book
					$(logInfo) $ Text.concat [ "Insertion: ", userEmail $ entityVal $ aid , " -> ",Text.pack $ show book ]
					setMessageI $ MsgBookCreated $ vocabBookName book
					redirect $ VocabtrainR
				_ -> vocabLayout $ do
					setTitleI MsgBookPleaseCorrectEntry
					widgetVocabtrainBookCreate bookFormWidget bookFormEnctype

getVocabtrainBookDeleteR :: VocabBookId -> GHandler App App RepHtml
getVocabtrainBookDeleteR bookId = do
	book <- runDB $ get bookId
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/book_delete.hamlet") 

postVocabtrainBookDeleteR :: VocabBookId -> GHandler App App RepHtml
postVocabtrainBookDeleteR bookId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			mbook <- runDB $ get bookId
			case mbook of
				Just book -> do
					$(logInfo) $ Text.concat [ "Deletion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show book ]
					_ <- runDB $ delete bookId
					_ <- runDB $ insert $ VocabBookManip (entityKey aid) bookId USERMANIP_DELETE (vocabBookTimestamp book) ""
					runDB $ deleteCascadeWhere [VocabChapterBookId ==. bookId]
					setMessageI $ MsgBookDeleted $ vocabBookName book
					redirect $ VocabtrainR
				Nothing -> do
					setMessageI $ MsgBookNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey bookId
					redirect $ VocabtrainR

getVocabtrainBookUpdateR :: VocabBookId -> GHandler App App RepHtml
getVocabtrainBookUpdateR bookId = do
	book <- runDB $ get bookId
	(bookFormWidget, bookFormEnctype) <- generateFormPost $ renderDivs $ bookForm book
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/book_update.hamlet") 

postVocabtrainBookUpdateR :: VocabBookId -> GHandler App App RepHtml
postVocabtrainBookUpdateR bookId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, bookFormWidget), bookFormEnctype) <- runFormPost $ renderDivs $ bookForm Nothing
			case result of
				FormSuccess book -> do -- vocabLayout [whamlet|<p>#{show book}|]
					_ <- runDB $ replace bookId book
					$(logInfo) $ Text.concat [ "manipulation: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show book ]
					_ <- runDB $ insert $ VocabBookManip (entityKey aid) bookId USERMANIP_UPDATE (vocabBookTimestamp book) $ vocabBookName book
					setMessageI $ MsgBookUpdated $ vocabBookName book
					redirect $ VocabtrainR
				_ -> do
					book <- runDB $ get bookId
					vocabLayout $ do
						setTitleI MsgBookPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/book_update.hamlet") 

getVocabtrainBookLogR :: VocabBookId -> GHandler App App RepHtml
getVocabtrainBookLogR bookId = do
	mbook <- runDB $ get bookId
	case mbook of
		Nothing -> notFound
		Just book -> do
			bookLogs <- runDB $ selectList [ VocabBookManipBookId ==. bookId] [Desc VocabBookManipTimestamp]
			vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/book_log.hamlet") 

widgetVocabtrainUserManipLog :: ToUserManipLog a => [Entity a] -> GWidget App App ()
widgetVocabtrainUserManipLog entityValues = do
	values <- mapM (lift . toUserManipLog . entityVal) entityValues
	toWidget $(whamletFile "templates/vocabtrain/log.hamlet") 

{-
-- Chapter Form
-}



chapterForm :: Maybe VocabChapter -> VocabBookId -> AForm App App VocabChapter
chapterForm mchapter bookId = VocabChapter
	<$> pure bookId
	<*> areq textField (fieldSettingsLabel MsgFieldVolume) (vocabChapterVolume <$> mchapter)

--widgetVocabtrainChapterCreate :: GWidget App App () -> Enctype -> GWidget App App ()
--widgetVocabtrainChapterCreate :: VocabBookId -> GWidget App App ()
--widgetVocabtrainChapterCreate chapterFormWidget chapterFormEnctype = toWidget $(whamletFile "templates/vocabtrain/chapter_create.hamlet") 
widgetVocabtrainChapterCreate' :: VocabBookId -> GWidget App App ()
widgetVocabtrainChapterCreate' bookId = do
	(chapterFormWidget, chapterFormEnctype) <- lift $ generateFormPost $ renderDivs $ chapterForm Nothing bookId
	widgetVocabtrainChapterCreate bookId chapterFormWidget chapterFormEnctype
--	toWidget $(whamletFile "templates/vocabtrain/chapter_create.hamlet") 

widgetVocabtrainChapterCreate :: VocabBookId -> GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainChapterCreate bookId chapterFormWidget chapterFormEnctype = toWidget $(whamletFile "templates/vocabtrain/chapter_create.hamlet") 

postVocabtrainChapterInsertR :: VocabBookId -> GHandler App App RepHtml
postVocabtrainChapterInsertR bookId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, chapterFormWidget), chapterFormEnctype) <- runFormPost $ renderDivs $ chapterForm Nothing bookId
			case result of
				FormSuccess chapter -> do 
					chapterId <- runDB $ insert chapter
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_INSERT time $ vocabChapterVolume chapter)
					$(logInfo) $ Text.concat [ "insertion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show chapter]
					setMessageI $ MsgChapterCreated $ vocabChapterVolume chapter
					redirect $ VocabtrainChapterR chapterId
				_ -> vocabLayout $ do
					setTitleI MsgChapterPleaseCorrectEntry
					widgetVocabtrainChapterCreate bookId chapterFormWidget chapterFormEnctype

getVocabtrainChapterDeleteR :: VocabChapterId -> GHandler App App RepHtml
getVocabtrainChapterDeleteR chapterId = do
	chapter <- runDB $ get chapterId
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/chapter_delete.hamlet") 

postVocabtrainChapterDeleteR :: VocabChapterId -> GHandler App App RepHtml
postVocabtrainChapterDeleteR chapterId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			mchapter <- runDB $ get chapterId
			case mchapter of
				Just chapter -> do
					_ <- runDB $ delete chapterId
					$(logInfo) $ Text.concat [ "deletion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show chapterId]
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_DELETE time "" )
					setMessageI $ MsgChapterDeleted $ vocabChapterVolume chapter
					redirect $ VocabtrainR
				_ -> do
					setMessageI $ MsgChapterNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey chapterId
					redirect $ VocabtrainR

getVocabtrainChapterUpdateR :: VocabChapterId -> GHandler App App RepHtml
getVocabtrainChapterUpdateR chapterId = do
	chapter <- runDB $ get chapterId
	(chapterFormWidget, chapterFormEnctype) <- generateFormPost $ renderDivs $ chapterForm chapter (vocabChapterBookId $ fromJust chapter)
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/chapter_update.hamlet") 

postVocabtrainChapterUpdateR :: VocabChapterId -> GHandler App App RepHtml
postVocabtrainChapterUpdateR chapterId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			chapterm <- runDB $ get chapterId 
			chapter <- runDB $ get chapterId
			((result, chapterFormWidget), chapterFormEnctype) <- runFormPost $ renderDivs $ chapterForm Nothing (vocabChapterBookId $ fromJust chapter)
			case result of
				FormSuccess chapter' -> do 
					$(logInfo) $ Text.concat [ "manipulation: ", userEmail $ entityVal $ aid , " -> ",Text.pack $ show chapter', " old: ", Text.pack $ show chapter ]
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_UPDATE time $ vocabChapterVolume chapter')
					_ <- runDB $ replace chapterId chapter'
					setMessageI $ MsgChapterUpdated $ vocabChapterVolume chapter'
					redirect $ VocabtrainChapterR chapterId
				_ -> vocabLayout $ do
						setTitleI MsgChapterPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/chapter_update.hamlet") 

getVocabtrainOrphanedCardsR :: GHandler App App RepHtml
getVocabtrainOrphanedCardsR = do
	msgShow <- getMessageRender
	maid <- maybeAuth
	cardResults <- runDB $ rawSql
		"SELECT ?? FROM cards WHERE NOT EXISTS ( SELECT 1 FROM content WHERE content_card_id = cards._id);"
		[]
		:: GHandler App App [Entity VocabCard]
	translationResults <- forM cardResults (\cardEntity -> runDB $ selectList [VocabTranslationCardId ==. (entityKey $ cardEntity)] [Asc VocabTranslationContent])
	results <- return $ Prelude.zip cardResults translationResults
	setUltDestCurrent
	let cardListWidget = toWidget $(whamletFile "templates/vocabtrain/card_list.hamlet")
		where cardListExtraButtonWidget = Nothing :: Maybe ( Entity VocabCard -> GWidget App App ())
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/card_orphaned.hamlet")

getVocabtrainChapterR :: VocabChapterId -> GHandler App App RepHtml
getVocabtrainChapterR chapterId = do
	msgShow <- getMessageRender
	maid <- maybeAuth
	chapterm <- runDB $ get chapterId 
	let chapter =  fromJust chapterm
	bookm <- runDB $ get $ vocabChapterBookId chapter
	let book = fromJust bookm
	cardResults <- runDB $ rawSql
		"SELECT ?? FROM cards JOIN content ON content_card_id = cards._id JOIN chapters ON content_chapter_id = chapters._id WHERE chapters._id = ?;"
		[toPersistValue  chapterId]
		:: GHandler App App [Entity VocabCard]
	chapterResults <- runDB $ rawSql -- needed for navchapter.hamlet
		"SELECT ?? FROM chapters JOIN chapters self ON chapters.chapter_book_id = self.chapter_book_id WHERE self._id = ?;"
		[toPersistValue  chapterId]
		:: GHandler App App [Entity VocabChapter]
	translationResults <- forM cardResults (\cardEntity -> runDB $ selectList [VocabTranslationCardId ==. (entityKey $ cardEntity)] [Asc VocabTranslationContent])
	results <- return $ Prelude.zip cardResults translationResults
	
	setUltDestCurrent
	widgetChapter <- widgetToPageContent $ widgetVocabtrainChapterCreate' $ vocabChapterBookId chapter
	let vocabChapterLayout widget = globalLayout $ (vocabLayoutSheet widget) { 
				 sheetNav = Just $(whamletFile "templates/vocabtrain/navchapter.hamlet")
				}
	let cardListExtraButtonWidget = Just $ widgetCardListTatoebaSearch $ vocabBookLanguage book
	let cardListWidget = toWidget $(whamletFile "templates/vocabtrain/card_list.hamlet")
	vocabChapterLayout $ toWidget $(whamletFile "templates/vocabtrain/chapter.hamlet")

widgetCardListTatoebaSearch :: TatoebaLanguage -> Entity VocabCard -> GWidget App App ()
widgetCardListTatoebaSearch language card = toWidget $(whamletFile "templates/vocabtrain/card_list_icon_tatoeba.hamlet")

getVocabtrainCardR :: VocabCardId -> GHandler App App RepHtml
getVocabtrainCardR cardId = do
	setUltDestCurrent
	msgShow <- getMessageRender
	mcard <- runDB $ get cardId
	case mcard of
		Nothing -> notFound
		Just card -> do
			translationResults <- runDB $ selectList [VocabTranslationCardId ==. cardId] [Asc VocabTranslationContent]
			contentResults <- runDB $ selectList [VocabContentCardId ==. cardId] []
				
			chapterBookResult <- runDB $ runJoin (selectOneMany (VocabChapterBookId <-.) vocabChapterBookId)
				{ somOrderOne = [Asc VocabBookName]
				, somFilterMany = [ VocabChapterId <-. (map (vocabContentChapterId . entityVal) contentResults) ]
				}
			let vocabCardLayout widget = globalLayout $ (vocabLayoutSheet widget) { 
						 sheetNav = Just $(whamletFile "templates/vocabtrain/navcard.hamlet")
						}
			vocabCardLayout $ toWidget $(whamletFile "templates/vocabtrain/card.hamlet") 

{-
getVocabtrainCardR :: VocabCardId -> GHandler App App RepHtml
getVocabtrainCardR cardId = do
	setUltDestCurrent
	msgShow <- getMessageRender
	mcard <- runDB $ get cardId
	case mcard of
		Nothing -> notFound
		Just card -> do
			translationResults <- runDB $ selectList [VocabTranslationCardId ==. cardId] [Asc VocabTranslationContent]
			chapterResults <- runDB $ rawSql -- needed for navchapter.hamlet
				"SELECT ?? FROM chapters JOIN content ON chapters._id = content_chapter_id WHERE content_card_id = ?;"
				[toPersistValue cardId]
				:: GHandler App App [Entity VocabChapter]
			let vocabCardLayout widget = globalLayout $ SheetLayout { 
						  sheetTitle = sheetTitle $ vocabLayoutSheet widget
						, sheetNav = Just $(ihamletFile "templates/vocabtrain/navcard.hamlet")
						, sheetBanner =  sheetBanner $ vocabLayoutSheet widget
						, sheetContent = sheetContent $ vocabLayoutSheet widget
						}
			vocabCardLayout $ toWidget $(whamletFile "templates/vocabtrain/card.hamlet") 

-}

getVocabtrainChapterLogR :: VocabChapterId -> GHandler App App RepHtml
getVocabtrainChapterLogR chapterId = do
	mchapter <- runDB $ get chapterId
	case mchapter of
		Nothing -> notFound
		Just chapter -> do
			chapterLogs <- runDB $ selectList [ VocabChapterManipChapterId ==. chapterId] [Desc VocabChapterManipTimestamp]
			vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/chapter_log.hamlet") 

{-
-- Translation Form
-}


translationForm :: Maybe VocabTranslation -> VocabCardId -> AForm App App VocabTranslation
translationForm mtranslation cardId = VocabTranslation
	<$> pure cardId
	<*> areq (selectField getTatoebaLanguageOptionList) (fieldSettingsLabel MsgFieldLanguage) (vocabTranslationLanguage <$> mtranslation)
	<*> areq textField (fieldSettingsLabel MsgFieldTranslation) (vocabTranslationContent <$> mtranslation)
	<*> aopt textField (fieldSettingsLabel MsgFieldComment) (vocabTranslationComment <$> mtranslation)

widgetVocabtrainTranslationCreate' :: VocabCardId -> GWidget App App ()
widgetVocabtrainTranslationCreate' cardId = do
	(translationFormWidget, translationFormEnctype) <- lift $ generateFormPost $ renderDivs $ translationForm Nothing cardId
	widgetVocabtrainTranslationCreate cardId translationFormWidget translationFormEnctype

widgetVocabtrainTranslationCreate :: VocabCardId -> GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainTranslationCreate cardId translationFormWidget translationFormEnctype = toWidget $(whamletFile "templates/vocabtrain/translation_create.hamlet") 

postVocabtrainTranslationInsertR :: VocabCardId -> GHandler App App RepHtml
postVocabtrainTranslationInsertR cardId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, translationFormWidget), translationFormEnctype) <- runFormPost $ renderDivs $ translationForm Nothing cardId
			case result of
				FormSuccess translation -> do 
					translationId <- runDB $ insert translation
					$(logInfo) $ Text.concat [ "insertion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show translation]
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabTranslationManip (entityKey aid) translationId USERMANIP_INSERT time $ 
						Text.intercalate " ," $ catMaybes $ map (\c -> c translation) [Just . vocabTranslationContent, vocabTranslationComment]
						)
					setMessageI $ MsgTranslationCreated $ vocabTranslationContent translation
					redirectUltDest VocabtrainR
				_ -> vocabLayout $ do
					setTitleI MsgTranslationPleaseCorrectEntry
					widgetVocabtrainTranslationCreate cardId translationFormWidget translationFormEnctype

getVocabtrainTranslationDeleteR :: VocabTranslationId -> GHandler App App RepHtml
getVocabtrainTranslationDeleteR translationId = do
	translation <- runDB $ get translationId
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/translation_delete.hamlet") 

postVocabtrainTranslationDeleteR :: VocabTranslationId -> GHandler App App RepHtml
postVocabtrainTranslationDeleteR = deleteVocabtrainTranslationR
deleteVocabtrainTranslationR :: VocabTranslationId -> GHandler App App RepHtml
deleteVocabtrainTranslationR translationId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			mtranslation <- runDB $ get translationId
			case mtranslation of
				Just translation -> do
					$(logInfo) $ Text.concat [ "deletion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show translation]
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabTranslationManip (entityKey aid) translationId USERMANIP_DELETE time "")
					_ <- runDB $ delete translationId
					setMessageI $ MsgTranslationDeleted $ vocabTranslationContent translation
					redirectUltDest VocabtrainR
				_ -> do
					setMessageI $ MsgTranslationNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey translationId
					redirectUltDest VocabtrainR

getVocabtrainTranslationUpdateR :: VocabTranslationId -> GHandler App App RepHtml
getVocabtrainTranslationUpdateR translationId = do
	translation <- runDB $ get translationId
	(translationFormWidget, translationFormEnctype) <- generateFormPost $ renderDivs $ translationForm translation (vocabTranslationCardId $ fromJust translation)
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/translation_update.hamlet") 

postVocabtrainTranslationUpdateR :: VocabTranslationId -> GHandler App App RepHtml
postVocabtrainTranslationUpdateR translationId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			translation <- runDB $ get translationId
			((result, translationFormWidget), translationFormEnctype) <- runFormPost $ renderDivs $ translationForm Nothing (vocabTranslationCardId $ fromJust translation)
			case result of
				FormSuccess translation' -> do 
					_ <- runDB $ replace translationId translation'
					$(logInfo) $ Text.concat [ "manipulation: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show translation', " old: ", Text.pack $ show translation ]
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabTranslationManip (entityKey aid) translationId USERMANIP_UPDATE time $ 
						Text.intercalate " ," $ catMaybes $ map (\c -> c translation') [Just . vocabTranslationContent, vocabTranslationComment]
						)
					setMessageI $ MsgTranslationUpdated $ vocabTranslationContent translation'
					redirectUltDest VocabtrainR
				_ -> vocabLayout $ do
						setTitleI MsgTranslationPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/translation_update.hamlet") 

getVocabtrainTranslationLogR :: VocabTranslationId -> GHandler App App RepHtml
getVocabtrainTranslationLogR translationId = do
	mtranslation <- runDB $ get translationId
	case mtranslation of
		Nothing -> notFound
		Just translation -> do
			translationLogs <- runDB $ selectList [ VocabTranslationManipTranslationId ==. translationId] [Desc VocabTranslationManipTimestamp]
			vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/translation_log.hamlet") 

{-
-- Card Form
-}

cardForm :: Maybe VocabCard -> AForm App App VocabCard
cardForm mcard = VocabCard 
	<$> areq textField ( (fieldSettingsLabel MsgFieldScript) { fsId = Just "cardFormScript"} ) (vocabCardScript <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldScriptComment) (vocabCardScriptComment <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeech) (vocabCardSpeech <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeechComment) (vocabCardSpeechComment <$> mcard)
	<*> areq (selectField cardTypePairs)   (fieldSettingsLabel MsgFieldCardType) (vocabCardType <$> mcard)
	where
		cardTypePairs = do
			--optionsPairs $ Prelude.map (Text.pack . show &&& Prelude.id) $
			msgShow <- getMessageRender
--			$(logInfo) $ Text.pack $ show $ map (\l -> calcCardTypeList l) $ concat $ map (\pri -> map (\l -> l ++ (priToList pri) ) $ sequence $ map (\x -> [0..x]) $ getCardTypeBounds' pri) ([minBound..maxBound] :: [CardTypePrimary])
			optionsPairs $ Prelude.map (getCardTypeText msgShow &&& Prelude.id) $ getAllCardTypes
--				(map (\l -> toEnum $ calcCardTypeList l) $ concat $ map (\pri -> map (\l -> l ++ (priToList pri) ) $ sequence $ map (\x -> [0..x]) $ getCardTypeBounds' pri) ([minBound..maxBound] :: [CardTypePrimary]) :: [CardType])
--			createVocabCard script scriptComment speech speechComment primaryType secondaryType tertiaryType =
--				VocabCard script scriptComment speech speechComment (CardType primaryType secondaryType tertiaryType)

{-
cardForm :: Maybe VocabCard -> AForm App App VocabCard
cardForm mcard = createVocabCard 
	<$> areq textField (fieldSettingsLabel MsgFieldScript) (vocabCardScript <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldScriptComment) (vocabCardScriptComment <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeech) (vocabCardSpeech <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeechComment) (vocabCardSpeechComment <$> mcard)
	<*> areq (selectFieldList cardTypePrimaryList)   (fieldSettingsLabel MsgFieldCardTypePrimary) (getCardTypePart cardTypePrimary)
	<*> areq (selectFieldList cardTypeSecondaryList) (fieldSettingsLabel MsgFieldCardTypeSecondary) (getCardTypePart cardTypeSecondary)
	<*> areq (selectFieldList cardTypeTertiaryList)  (fieldSettingsLabel MsgFieldCardTypeTertiary) (getCardTypePart cardTypeTertiary)
		where
			getCardTypePart :: (CardType -> a) -> Maybe a
			getCardTypePart partCons = (maybe Nothing (Just . partCons) (vocabCardType <$> mcard))
			createVocabCard script scriptComment speech speechComment primaryType secondaryType tertiaryType =
				VocabCard script scriptComment speech speechComment (CardType primaryType secondaryType tertiaryType)
			cardTypePrimaryList :: [(Text, CardTypePrimary)]
			cardTypePrimaryList = map (Text.pack . show &&& id) $ [minBound..maxBound] 
			cardTypeSecondaryList :: [(Text, CardTypeSecondary)]
			cardTypeSecondaryList = map (Text.pack . show &&& id) $ [minBound..maxBound] 
			cardTypeTertiaryList :: [(Text, CardTypeTertiary)]
			cardTypeTertiaryList = map (Text.pack . show &&& id) $ [minBound..maxBound] 
-}

widgetVocabtrainCardCreate' :: VocabChapterId -> GWidget App App ()
widgetVocabtrainCardCreate' chapterId = do
	(cardFormWidget, cardFormEnctype) <- lift $ generateFormPost $ renderDivs $ cardForm Nothing 
	widgetVocabtrainCardCreate chapterId cardFormWidget cardFormEnctype

widgetVocabtrainCardCreate :: VocabChapterId -> GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainCardCreate chapterId cardFormWidget cardFormEnctype = do
	res <- r
	case res of 
		Just a -> a
		Nothing -> lift $ notFound
	where r = runMaybeT $ do 
		chapter <- MaybeT . lift . runDB . get $ chapterId
		book <- MaybeT . lift . runDB . get . vocabChapterBookId $ chapter
		return $ toWidget $(widgetFile "vocabtrain/card_create")

postVocabtrainCardInsertR :: VocabChapterId -> GHandler App App RepHtml
postVocabtrainCardInsertR chapterId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, cardFormWidget), cardFormEnctype) <- runFormPost $ renderDivs $ cardForm Nothing
			case result of
				FormSuccess card -> do 
					duplicateCards <- runDB $ selectList 
						[ VocabCardScript ==. (vocabCardScript card)
						, VocabCardScriptComment ==. (vocabCardScriptComment card)
						, VocabCardSpeech ==. (vocabCardSpeech card)
						, VocabCardSpeechComment ==. (vocabCardSpeechComment card)
						] []
					if length duplicateCards == 0
						then do
							cardId <- runDB $ insert card
							_ <- runDB $ insert $ VocabContent chapterId cardId
							$(logInfo) $ Text.concat [ "insertion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card]
							_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabCardManip (entityKey aid) cardId USERMANIP_INSERT time $ 
								Text.intercalate " ," $ catMaybes $ map (\c -> c card) [Just . vocabCardScript, vocabCardScriptComment, vocabCardSpeech, vocabCardSpeechComment]
								)
							setMessageI $ MsgCardCreated $ vocabCardScript card
							redirect $ VocabtrainChapterR chapterId 
						else do
							let dup = duplicateCards !! 0
							_ <- runDB $ insert $ VocabContent chapterId (entityKey dup) -- TODO: perhaps already in this chapter?
							$(logInfo) $ Text.concat [ "Card added: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card, " To chapter ", Text.pack $ show chapterId]
							_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_PUT time $ keyToText $ entityKey dup)
							setMessageI $ MsgCardChapterAdded $ vocabCardScript card
							redirect $ VocabtrainChapterR chapterId 
				_ -> vocabLayout $ do
					setTitleI MsgCardPleaseCorrectEntry
					widgetVocabtrainCardCreate chapterId cardFormWidget cardFormEnctype


keyToText :: KeyBackend backend entity -> Text
keyToText = fromRightText . fromPersistValue . unKey

getVocabtrainCardDeleteR :: VocabCardId -> GHandler App App RepHtml
getVocabtrainCardDeleteR cardId = do
	card <- runDB $ get cardId
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/card_delete.hamlet") 

postVocabtrainCardDeleteR :: VocabCardId -> GHandler App App RepHtml
postVocabtrainCardDeleteR cardId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			mcard <- runDB $ get cardId
			case mcard of
				Just card -> do
					$(logInfo) $ Text.concat [ "deletion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card]
					_ <- runDB $ delete cardId
					_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabCardManip (entityKey aid) cardId USERMANIP_DELETE time "")
					setMessageI $ MsgCardDeleted $ vocabCardScript card
					redirectUltDest VocabtrainR
				_ -> do
					setMessageI $ MsgCardNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey cardId
					redirectUltDest VocabtrainR

cardChapterForm :: VocabCardId -> AForm App App [VocabChapterId]
cardChapterForm cardId = areq (multiSelectField chapters) (fieldSettingsLabel MsgFieldChapters) Nothing
	where
		chapters :: GHandler App App (OptionList VocabChapterId)
		chapters = do -- use optionsPersist ?
			contentResults <- runDB $ selectList [VocabContentCardId ==. cardId] []
			chapterBookResult <- runDB $ runJoin (selectOneMany (VocabChapterBookId <-.) vocabChapterBookId)
				{ somOrderOne = [Asc VocabBookName]
				, somFilterMany = [ VocabChapterId <-. (map (vocabContentChapterId . entityVal) contentResults) ]
				}
			optionsPairs $ List.concat $ Prelude.map 
				(\(book,chapterList) -> 
					Prelude.map (\chapter -> 
						(Text.concat [ vocabBookName $ entityVal book, ": ", vocabChapterVolume $ entityVal chapter ] , entityKey chapter) ) 
						chapterList
				) chapterBookResult
{-
			chapterResults <- runDB $ rawSql
				"SELECT ?? FROM chapters JOIN content ON content_chapter_id = chapters._id JOIN cards ON content_card_id = cards._id WHERE cards._id = ?;"
				[toPersistValue  cardId]
				:: GHandler App App [Entity VocabChapter]
			optionsPairs $ Prelude.map (vocabChapterVolume . entityVal &&& entityKey) chapterResults
-}
getVocabtrainCardUpdateR :: VocabCardId -> GHandler App App RepHtml
getVocabtrainCardUpdateR cardId = do
	card <- runDB $ get cardId
	(cardFormWidget, cardFormEnctype) <- generateFormPost $ renderDivs $ cardForm card
	(cardChapterFormWidget, cardChapterFormEnctype) <- generateFormPost $ renderDivs $ cardChapterForm cardId
	vocabLayout $ do
		toWidget $(whamletFile "templates/vocabtrain/card_update.hamlet") 
		toWidget $(whamletFile "templates/vocabtrain/cardchapter_update.hamlet") 

postVocabtrainCardUpdateR :: VocabCardId -> GHandler App App RepHtml
postVocabtrainCardUpdateR cardId = do
	msgShow <- getMessageRender
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			card <- runDB $ get cardId
			((result, cardFormWidget), cardFormEnctype) <- runFormPost $ renderDivs $ cardForm card
			case result of
				FormSuccess card' -> do 
					duplicateCards <- runDB $ selectList 
						[ VocabCardScript ==. (vocabCardScript card')
						, VocabCardScriptComment ==. (vocabCardScriptComment card')
						, VocabCardSpeech ==. (vocabCardSpeech card')
						, VocabCardSpeechComment ==. (vocabCardSpeechComment card')
						, VocabCardId !=. cardId
						] []
					if length duplicateCards == 0
						then do
							_ <- runDB $ replace cardId card'
							$(logInfo) $ Text.concat [ "manipulation: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card', " old: ", Text.pack $ show card]
							_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabCardManip (entityKey aid) cardId USERMANIP_UPDATE time $ 
								Text.intercalate " ," $ catMaybes $ map (\c -> c card') [Just . vocabCardScript, vocabCardScriptComment, vocabCardSpeech, vocabCardSpeechComment]
								)
							setMessageI $ MsgCardUpdated $ vocabCardScript card'
							redirectUltDest VocabtrainR
						else invalidArgs [msgShow MsgCardUpdateToDuplicate]
				_ -> vocabLayout $ do
						setTitleI MsgCardPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/card_update.hamlet") 

postVocabtrainCardChaptersDeleteR :: VocabCardId -> GHandler App App RepHtml
postVocabtrainCardChaptersDeleteR cardId = do
	card <- runDB $ get cardId
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, cardChapterFormWidget), cardChapterFormEnctype) <- runFormPost $ renderDivs $ cardChapterForm cardId
			case result of
				FormSuccess cardChapters -> do 
					_ <- runDB $ deleteWhere [VocabContentChapterId <-. cardChapters, VocabContentCardId ==. cardId ]
					$(logInfo) $ Text.concat [ "Deleted Card from Chapters: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card, " chapters: ", Text.pack $ show cardChapters]
					time <- liftIO getCurrentTime
					mapM_ (\chapterId -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_REMOVE time $ keyToText cardId) cardChapters
					setMessageI $ MsgCardChaptersDeleted $ vocabCardScript $ fromJust card
					redirect $ VocabtrainR
				_ -> vocabLayout $ do
						setTitleI MsgCardPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/cardchapter_update.hamlet") 

getVocabtrainCardLogR :: VocabCardId -> GHandler App App RepHtml
getVocabtrainCardLogR cardId = do
	mcard <- runDB $ get cardId
	case mcard of
		Nothing -> notFound
		Just card -> do
			cardLogs <- runDB $ selectList [ VocabCardManipCardId ==. cardId] [Desc VocabCardManipTimestamp]
			vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/card_log.hamlet") 


cardSearchForm :: Text -> AForm App App Text
cardSearchForm queryString = areq (searchField True) ((fieldSettingsLabel MsgFieldSearchCard) { fsLabel = "", fsAttrs = [("placeholder", queryString), ("class", "search-query")] }) Nothing



cardSearchFormWidget :: GWidget App App ()
cardSearchFormWidget = do
	msgShow <- lift $ getMessageRender
	(widget, _) <- lift $ generateFormPost $ renderDivs $ cardSearchForm $ msgShow MsgFieldSearchCard
	widget


cardQueryForm :: AForm App App Text
cardQueryForm = areq (searchField True) (fieldSettingsLabel MsgFieldSearchCard) Nothing
-- cardQueryForm = areq (searchField True) (FieldSettings { fsLabel = fsLabel $ fieldSettingsLabel MsgFieldSearchCard, fsAttrs = [("class", "search-query")] } ) Nothing

{-
defaultCardQueryFormWidget :: GWidget App App ()
defaultCardQueryFormWidget = do
	((_, cardQueryFormWidget), _) <- lift $ generateFormGet $ renderDivs $ cardQueryForm
	cardQueryFormWidget
-}

widgetVocabtrainCardChapterAddSearch' :: VocabChapterId -> GWidget App App ()
widgetVocabtrainCardChapterAddSearch' chapterId = do
	((_, cardQueryFormWidget), cardQueryFormEnctype) <- lift $ generateFormGet $ renderDivs $ cardQueryForm
	widgetVocabtrainCardChapterAddSearch chapterId cardQueryFormWidget cardQueryFormEnctype

widgetVocabtrainCardChapterAddSearch :: VocabChapterId -> GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainCardChapterAddSearch chapterId cardQueryFormWidget cardQueryFormEnctype = toWidget $(whamletFile "templates/vocabtrain/cardchapter_add_search.hamlet") 


icontains :: EntityField v Text -> Text -> Filter v
icontains field val = Filter field (Left $ Text.concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")

icontainsMaybe :: EntityField v (Maybe Text) -> Text -> Filter v
icontainsMaybe field val = Filter field (Left $ Just $ Text.concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")

{-
getVocabtrainCardChapterAddR :: VocabChapterId -> Handler RepHtml
getVocabtrainCardChapterAddR chapterId = do
	msgShow <- getMessageRender
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result', cardQueryFormWidget), cardQueryFormEnctype) <- runFormGet $ renderDivs $ cardQueryForm
			case result' of
				FormSuccess searchPhrase -> do 
					cardResults <- runDB $ selectList 
						(     [ icontains VocabCardScript        searchPhrase ]
						  ||. [ icontainsMaybe VocabCardSpeech   searchPhrase ]
						) []
					vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/cardchapter_add_list.hamlet")
				_ -> vocabLayout $ do
						setTitleI MsgCardPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/cardchapter_add_search.hamlet") 
-}
getVocabtrainCardChapterAddR :: VocabChapterId -> Handler RepHtml
getVocabtrainCardChapterAddR chapterId = do
	msgShow <- getMessageRender
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result', cardQueryFormWidget), cardQueryFormEnctype) <- runFormGet $ renderDivs $ cardQueryForm
			case result' of
				FormSuccess searchPhrase -> do
					res <- r
					case res of 
						Just a -> a
						Nothing -> notFound
					where r = runMaybeT $ do 
						chapter <- MaybeT . runDB . get $ chapterId
						book <- MaybeT . runDB . get . vocabChapterBookId $ chapter
						return $ getVocabtrainCardSearch (vocabBookLanguage book) searchPhrase (Just $ widgetCardListChapterAdd chapterId)
				_ -> vocabLayout $ do
						setTitleI MsgCardPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/cardchapter_add_search.hamlet") 

widgetCardListChapterAdd :: VocabChapterId -> Entity VocabCard -> GWidget App App ()
widgetCardListChapterAdd chapterId card = do toWidget $(whamletFile "templates/vocabtrain/card_list_icon_chapteradd.hamlet")

postVocabtrainCardChapterInsertR :: VocabCardId -> VocabChapterId -> Handler RepHtml
postVocabtrainCardChapterInsertR cardId chapterId = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			_ <- runDB $ insert $ VocabContent chapterId cardId
			$(logInfo) $ Text.concat [ "Added link chapter<->card: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show chapterId, Text.pack $ show cardId]
			_ <- (liftIO getCurrentTime >>= \time -> runDB $ insert $ VocabChapterManip (entityKey aid) chapterId USERMANIP_PUT time $ keyToText cardId)
			setMessageI $ MsgCardChapterCreated 
			redirect $ VocabtrainChapterR chapterId -- TODO TranslationR translationId


postVocabtrainCardSearchR :: GHandler App App RepHtml
postVocabtrainCardSearchR = do
	msgShow <- getMessageRender
	((result, _), _) <- runFormPost $ renderDivs $ cardSearchForm $ msgShow MsgFieldSearchCard
	case result of
		FormSuccess searchPhrase -> do 
			getVocabtrainCardSearch LANG_UND searchPhrase Nothing
		_ -> vocabLayout $ do
			lift $ setMessageI MsgCardPleaseCorrectEntry
			lift $ redirectUltDest VocabtrainR
			
getVocabtrainCardQueryR :: TatoebaLanguage -> Text -> Handler RepHtml
getVocabtrainCardQueryR language searchPhrase = getVocabtrainCardSearch language searchPhrase Nothing

getVocabtrainCardSearch :: TatoebaLanguage -> Text -> Maybe ( Entity VocabCard -> GWidget App App ()) -> GHandler App App RepHtml
getVocabtrainCardSearch language searchPhrase cardListExtraButtonWidget = do
	msgShow <- getMessageRender
	maid <- maybeAuth 
	setUltDestCurrent
	cardResults <- vocabtrainCardQuery language searchPhrase
	{-
	cardResults <- runDB $ selectList 
		(     [ icontains VocabCardScript        searchPhrase ]
		  ||. [ icontainsMaybe VocabCardSpeech   searchPhrase ]
		) []
	-}
	translationResults <- forM cardResults 
		(\cardEntity -> runDB $ selectList [VocabTranslationCardId ==. (entityKey $ cardEntity)] [Asc VocabTranslationContent])
	results <- return $ Prelude.zip cardResults translationResults
	let cardListWidget = toWidget $(whamletFile "templates/vocabtrain/card_list.hamlet")
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/card_search.hamlet")
{-
		where 
			cardListExtraButtonWidget :: Maybe ( Entity VocabCard -> GWidget App App ())
			cardListExtraButtonWidget = Nothing
-}

{-	results <- mapM (\cardResult -> do
			runDB $ 
			runDB $ C.runResourceT $ withStmt
				"SELECT translation_language from translations join cards on translation_card_id = cards._id where cards._id = ?;"
				[entityKey $ cardResult] C.$$ CL.consume
			) cardResults
-}			
--	translationResults <- selectList [VocabTranslationCardId <-. ( map (vocabContentCardId . entityVal) cardResult)] [] 
{-	vocabLayout [whamlet|
$forall result <- results
   <li>#{vocabCardScript $ entityVal $ fst result}
   $forall lang <- snd result
      <li>#{vocabTranslationLanguage $ entityVal lang}
|]
-}
{-
	vocabLayout [whamlet|
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
	vocabLayout $ sequence $ forM bookResult (\bookEntity -> do
--		bookName <- vocabBookName $ entityVal bookEntity
		toWidget [whamlet|<a href=@{HomeR}>Go home!|]
		)
--	liftIO $ print $ map (vocabBookName . entityVal) bookResult
	--liftIO $ print (bookResult :: [Entity bookResult])
	--chapterResult <- runDB $ selectList [ VocabChapterBookId <-. (map (\i -> Key $ toPersistValue i) (requestBooks request))] []
--	val <- VocabBookName $ entityVal $ (bookResult!!0)
--	vocabLayout [whamlet|<a href=@{HomeR}>Go home!|]
-}	

{-getVocabtrainBookSupplyR :: Handler RepJson
getVocabtrainBookSupplyR = jsonToRepJson $ JS.toJSON ([5]::[Int])
-}

--postVocabtrainBookSupplyR :: Handler RepPlain


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


tatoebaLanguageWidget :: TatoebaLanguage -> GWidget App App()
tatoebaLanguageWidget lang = do
	msgShow <- lift $ getMessageRender
	toWidget $ [hamlet|<i .flag-#{Text.pack $ show $ lang} title=#{msgShow $ toAppMessage lang} data-placement="bottom" rel="tooltip"> |]


-- Account

getUserR :: UserId -> GHandler App App RepHtml
getUserR userId = do
	maid <- maybeAuth
	muser <- runDB $ get userId
	case muser of
		Nothing -> do
			msgShow <- getMessageRender
			invalidArgs [msgShow $ MsgUserIdNotFound userId]
		Just user -> do
			vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/user.hamlet")
	


userForm :: Text -> Maybe User -> AForm App App User
userForm email muser = User
	<$> pure email
	<*> aopt textField (fieldSettingsLabel MsgFieldUserNick) (userNick <$> muser)
	<*> aopt passwordField (fieldSettingsLabel MsgFieldUserPassword) Nothing
	<*> pure Nothing


getUserUpdateR :: UserId -> GHandler App App RepHtml
getUserUpdateR userId = do
	maid <- maybeAuth
	muser <- runDB $ get userId
	case muser of
		Nothing -> do
			msgShow <- getMessageRender
			invalidArgs [msgShow $ MsgUserIdNotFound userId]
			--permissionDenied $ msgShow MsgUserManipulationPermissionDenied
		Just user -> do
			case maid of
				Nothing -> do
					msgShow <- getMessageRender
					permissionDenied $ msgShow MsgUserManipulationPermissionDenied
				Just (Entity _ userVal) -> do
					(userFormWidget, userFormEnctype) <- generateFormPost $ renderDivs $ userForm (userEmail user) (Just user)
					vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/user_update.hamlet")

postUserUpdateR :: UserId -> GHandler App App RepHtml
postUserUpdateR userId = do
	moldUser <- runDB $ get userId
	case moldUser of
		Nothing -> do
			msgShow <- getMessageRender
			invalidArgs [msgShow $ MsgUserIdNotFound userId]
		Just oldUser -> do
			maid <- maybeAuth 
			case maid of
				Nothing -> do
					msgShow <- getMessageRender
					permissionDenied $ msgShow MsgUserManipulationPermissionDenied
				Just aid -> do
					((result, userFormWidget), userFormEnctype) <- runFormPost $ renderDivs $ userForm (userEmail oldUser) (Just oldUser)
					case result of
						FormSuccess newUser -> do 
							admin <- isAdmin
							if admin /= Authorized && (userEmail newUser) /= (userEmail $ entityVal aid)
								then do
									msgShow <- getMessageRender
									permissionDenied $ msgShow MsgUserManipulationPermissionDenied
								else do
									if isNothing $ userNick newUser
										then do
											postUserUpdateR' userId newUser
										else do
											userResult <- runDB $ selectList [ UserNick ==. userNick newUser, UserId !=. userId] [] 
											if List.null userResult
												then postUserUpdateR' userId newUser
												else do
													msgShow <- getMessageRender
													permissionDenied $ msgShow MsgUserNickAlreadyForgiven
						_ ->  do
							setMessageI MsgPleaseCorrectEntry
							vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/user_update.hamlet")
-- postUserUpdateR' :: UserId -> UserGeneric SqlPersist -> GHandler App App RepHtml
postUserUpdateR' :: Key (UserGeneric SqlBackend) -> UserGeneric SqlBackend -> GHandler App App RepHtml
postUserUpdateR' userId newUser = do
	_ <- runDB $ replace userId newUser
	$(logInfo) $ Text.concat [ "Update: ", userEmail newUser]
	setMessageI $ maybe MsgNobodyUpdated (\t -> MsgUserUpdated t) $ userNick newUser
	redirect $ VocabtrainR



---

{-
getVocabtrainCardQueryR :: TatoebaLanguage -> Text -> Handler RepHtml
getVocabtrainCardQueryR language searchPhrase = do
	let cardListExtraButtonWidget = Nothing :: Maybe ( Entity VocabCard -> GWidget App App ())
	msgShow <- getMessageRender
	maid <- maybeAuth 
	cardResults <- vocabtrainCardQuery language searchPhrase
	translationResults <- forM cardResults 
		(\cardEntity -> runDB $ selectList [VocabTranslationCardId ==. (entityKey $ cardEntity)] [Asc VocabTranslationContent])
	results <- return $ Prelude.zip cardResults translationResults
	let cardListWidget = toWidget $(whamletFile "templates/vocabtrain/card_list.hamlet")
	vocabLayout $ toWidget $(whamletFile "templates/vocabtrain/card_search.hamlet")
-}	

vocabtrainCardQuery :: TatoebaLanguage -> Text -> GHandler App App [Entity VocabCard]
vocabtrainCardQuery language searchPhrase = do
	sphinxResult <- liftIO $ querySphinx (Text.concat [ "vocabtrain_" , Text.pack . show $ language]) searchPhrase
	runDB $ selectList [VocabCardId <-. ( map (\t -> (Key $ PersistInt64 t) :: VocabCardId) sphinxResult) ] []

getVocabtrainCardQueryJR :: TatoebaLanguage -> Text -> Handler RepJson
getVocabtrainCardQueryJR language searchPhrase = do
	cardResults <- vocabtrainCardQuery language searchPhrase
	jsonToRepJson $ map JS.toJSON cardResults

instance JS.ToJSON (Entity VocabCard) where
	toJSON ecard = JS.object 
		[ ("_id", JS.toJSON $ entityKey ecard)
		, ("card_script", JS.toJSON $ vocabCardScript card)
		, ("card_script_comment", JS.toJSON $ vocabCardScriptComment card)
		, ("card_speech", JS.toJSON $ vocabCardSpeech card)
		, ("card_speech_comment", JS.toJSON $ vocabCardSpeechComment card)
		]
		where card = entityVal ecard
