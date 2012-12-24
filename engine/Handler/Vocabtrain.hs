{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric #-}
module Handler.Vocabtrain where

import Import
import CardType
import qualified Prelude
import qualified Data.Text as Text
import Data.Aeson ((.:))
import qualified Data.Aeson as JS
import GHC.Generics

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally, IOException)
import Data.Maybe


import Control.Monad

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Database.Persist.GenericSql.Raw (withStmt)
import Database.Persist.GenericSql
import Database.Persist.Sqlite
import Database.Persist.Store
import Text.Hamlet

import Data.Conduit.Lazy
import qualified Data.Attoparsec.ByteString as JSP
import Network.Wai (requestBody)
import Data.Time (getCurrentTime) 

import Control.Arrow


vocabLayoutSheet :: GWidget App App () -> SheetLayout App (Route App)
vocabLayoutSheet widget = SheetLayout { 
	  sheetTitle = "Vocabtrain"
	, sheetNav = Nothing
	, sheetBanner =  Just $(ihamletFile "templates/vocabtrain/banner.hamlet") 
	, sheetContent = widget
	}

vocabLayout :: GWidget App App () -> GHandler App App RepHtml
vocabLayout widget = globalLayout $ vocabLayoutSheet widget

{-
-- Book Form
-}

bookForm :: Maybe VocabBook -> AForm App App VocabBook
bookForm mbook = VocabBook
	<$> areq textField (fieldSettingsLabel MsgFieldName) (vocabBookName <$> mbook)
	<*> areq (selectFieldList tatoebaLanguages) (fieldSettingsLabel MsgFieldLanguage) (vocabBookLanguage <$> mbook)
	<*> aformM (liftIO getCurrentTime)
	where
		tatoebaLanguages = map (getTatoebaLanguageName &&& Prelude.id) $ [(minBound::TatoebaLanguage)..(maxBound::TatoebaLanguage)]

widgetVocabtrainBookCreate :: GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainBookCreate bookFormWidget bookFormEnctype = toWidget $(whamletFile "templates/vocabtrain/book_create.hamlet") 

getVocabtrainBooksR :: GHandler App App RepHtml
getVocabtrainBooksR = do
	maid <- maybeAuth
	bookResult <- runDB $ selectList [] [Asc VocabBookName] --VocabBookId ==. (Key $ toPersistValue (1::Int)) ] [] 
	chapterResults <- forM bookResult (\bookEntity -> runDB $ selectList [VocabChapterBookId ==. (entityKey $ bookEntity)] [Asc VocabChapterVolume])
	results <- return $ Prelude.zip bookResult chapterResults -- $ forM bookResult (\bookEntity -> return $ runDB $ selectList [VocabChapterId ==. (entityKey $ bookEntity)] []) 
	let a = either (\_ -> ""::Text) Prelude.id $ fromPersistValue $ unKey $ entityKey $ (bookResult!!0)
	(bookFormWidget, bookFormEnctype) <- generateFormPost $ renderDivs $ bookForm Nothing

	widgetBook <- widgetToPageContent $ widgetVocabtrainBookCreate bookFormWidget bookFormEnctype
	let vocabBookLayout widget = globalLayout $ SheetLayout { 
				  sheetTitle = sheetTitle $ vocabLayoutSheet widget
				, sheetNav = Just $(ihamletFile "templates/vocabtrain/navbook.hamlet")
				, sheetBanner =  sheetBanner $ vocabLayoutSheet widget
				, sheetContent = sheetContent $ vocabLayoutSheet widget
				}
	vocabBookLayout $ toWidget $(whamletFile "templates/vocabtrain/books.hamlet") 



postVocabtrainBooksR :: GHandler App App RepHtml
postVocabtrainBooksR = do
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			msgShow <- getMessageRender
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			((result, bookFormWidget), bookFormEnctype) <- runFormPost $ renderDivs $ bookForm Nothing
			case result of
				FormSuccess book -> do -- vocabLayout [whamlet|<p>#{show book}|]
					_ <- runDB $ insert book
					$(logInfo) $ Text.concat [ "Insertion: ", userEmail $ entityVal $ aid , " -> ",Text.pack $ show book ]
					setMessageI $ MsgBookCreated $ vocabBookName book
					redirect $ VocabtrainBooksR
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
			book <- runDB $ get bookId
			if isJust book 
				then do
					$(logInfo) $ Text.concat [ "Deletion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show book ]
					_ <- runDB $ delete bookId
					runDB $ deleteCascadeWhere [VocabChapterBookId ==. bookId]
					setMessageI $ MsgBookDeleted $ vocabBookName $ fromJust book
					redirect $ VocabtrainBooksR
				else do
					setMessageI $ MsgBookNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey bookId
					redirect $ VocabtrainBooksR

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
					setMessageI $ MsgBookUpdated $ vocabBookName book
					redirect $ VocabtrainBooksR
				_ -> do
					book <- runDB $ get bookId
					vocabLayout $ do
						setTitleI MsgBookPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/book_update.hamlet") 

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
					setMessageI $ MsgChapterDeleted $ vocabChapterVolume chapter
					redirect $ VocabtrainBooksR
				_ -> do
					setMessageI $ MsgChapterNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey chapterId
					redirect $ VocabtrainBooksR

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
					_ <- runDB $ replace chapterId chapter'
					setMessageI $ MsgChapterUpdated $ vocabChapterVolume chapter'
					redirect $ VocabtrainChapterR chapterId
				_ -> vocabLayout $ do
						setTitleI MsgChapterPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/chapter_update.hamlet") 


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
	let vocabChapterLayout widget = globalLayout $ SheetLayout { 
				  sheetTitle = sheetTitle $ vocabLayoutSheet widget
				, sheetNav = Just $(ihamletFile "templates/vocabtrain/navchapter.hamlet")
				, sheetBanner =  sheetBanner $ vocabLayoutSheet widget
				, sheetContent = sheetContent $ vocabLayoutSheet widget
				}
	vocabChapterLayout $ toWidget $(whamletFile "templates/vocabtrain/chapter.hamlet") 

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


{-
-- Translation Form
-}


translationForm :: Maybe VocabTranslation -> VocabCardId -> AForm App App VocabTranslation
translationForm mtranslation cardId = VocabTranslation
	<$> pure cardId
	<*> areq (selectFieldList tatoebaLanguages) (fieldSettingsLabel MsgFieldLanguage) (vocabTranslationLanguage <$> mtranslation)
	<*> areq textField (fieldSettingsLabel MsgFieldTranslation) (vocabTranslationContent <$> mtranslation)
	<*> aopt textField (fieldSettingsLabel MsgFieldComment) (vocabTranslationComment <$> mtranslation)
	where
		tatoebaLanguages = map (getTatoebaLanguageName &&& Prelude.id) $ [(minBound::TatoebaLanguage)..(maxBound::TatoebaLanguage)]

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
					_ <- runDB $ insert translation
					$(logInfo) $ Text.concat [ "insertion: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show translation]
					setMessageI $ MsgTranslationCreated $ vocabTranslationContent translation
					redirectUltDest VocabtrainBooksR
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
					_ <- runDB $ delete translationId
					setMessageI $ MsgTranslationDeleted $ vocabTranslationContent translation
					redirectUltDest VocabtrainBooksR
				_ -> do
					setMessageI $ MsgTranslationNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey translationId
					redirectUltDest VocabtrainBooksR

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
					setMessageI $ MsgTranslationUpdated $ vocabTranslationContent translation'
					redirectUltDest VocabtrainBooksR
				_ -> vocabLayout $ do
						setTitleI MsgTranslationPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/translation_update.hamlet") 


{-
-- Card Form
-}

cardForm :: Maybe VocabCard -> AForm App App VocabCard
cardForm mcard = VocabCard 
	<$> areq textField (fieldSettingsLabel MsgFieldScript) (vocabCardScript <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldScriptComment) (vocabCardScriptComment <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeech) (vocabCardSpeech <$> mcard)
	<*> aopt textField (fieldSettingsLabel MsgFieldSpeechComment) (vocabCardSpeechComment <$> mcard)
	<*> areq (selectField cardTypePairs)   (fieldSettingsLabel MsgFieldCardType) (vocabCardType <$> mcard)
	where
		cardTypePairs = do
			--optionsPairs $ Prelude.map (Text.pack . show &&& Prelude.id) $
			msgShow <- getMessageRender
			optionsPairs $ Prelude.map (getCardTypeText msgShow &&& Prelude.id) $
				(map (\l -> toEnum $ calcCardTypeList l) $ concat $ map (\pri -> map (\l -> (fromEnum pri) : l) $ sequence $ map (\x -> [0..x]) $ getCardTypeBounds' pri) ([minBound..maxBound] :: [CardTypePrimary]) :: [CardType])
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
widgetVocabtrainCardCreate chapterId cardFormWidget cardFormEnctype = toWidget $(whamletFile "templates/vocabtrain/card_create.hamlet") 

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
							setMessageI $ MsgCardCreated $ vocabCardScript card
							redirect $ VocabtrainChapterR chapterId 
						else do
							let dup = duplicateCards !! 0
							_ <- runDB $ insert $ VocabContent chapterId (entityKey dup)
							$(logInfo) $ Text.concat [ "Card added: ", userEmail $ entityVal $ aid , " -> ", Text.pack $ show card, " To chapter ", Text.pack $ show chapterId]
							setMessageI $ MsgCardChapterAdded $ vocabCardScript card
							redirect $ VocabtrainChapterR chapterId 
				_ -> vocabLayout $ do
					setTitleI MsgCardPleaseCorrectEntry
					widgetVocabtrainCardCreate chapterId cardFormWidget cardFormEnctype

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
					setMessageI $ MsgCardDeleted $ vocabCardScript card
					redirectUltDest VocabtrainBooksR
				_ -> do
					setMessageI $ MsgCardNotFound $ either (\_ -> (-1)::Int) Prelude.id $ fromPersistValue $ unKey cardId
					redirectUltDest VocabtrainBooksR

cardChapterForm :: VocabCardId -> AForm App App [VocabChapterId]
cardChapterForm cardId = areq (multiSelectField chapters) (fieldSettingsLabel MsgFieldChapters) Nothing
	where
		chapters :: GHandler App App (OptionList VocabChapterId)
		chapters = do -- use optionsPersist ?
			chapterResults <- runDB $ rawSql
				"SELECT ?? FROM chapters JOIN content ON content_chapter_id = chapters._id JOIN cards ON content_card_id = cards._id WHERE cards._id = ?;"
				[toPersistValue  cardId]
				:: GHandler App App [Entity VocabChapter]
			optionsPairs $ Prelude.map (vocabChapterVolume . entityVal &&& entityKey) chapterResults

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
							setMessageI $ MsgCardUpdated $ vocabCardScript card'
							redirectUltDest VocabtrainBooksR
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
					setMessageI $ MsgCardChaptersDeleted $ vocabCardScript $ fromJust card
					redirect $ VocabtrainBooksR
				_ -> vocabLayout $ do
						setTitleI MsgCardPleaseCorrectEntry
						toWidget $(whamletFile "templates/vocabtrain/cardchapter_update.hamlet") 

cardSearchForm :: AForm App App Text
cardSearchForm = areq (searchField True) (fieldSettingsLabel MsgFieldSearchCard) Nothing

widgetVocabtrainCardChapterAddSearch' :: VocabChapterId -> GWidget App App ()
widgetVocabtrainCardChapterAddSearch' chapterId = do
	((_, cardSearchFormWidget), cardSearchFormEnctype) <- lift $ runFormGet $ renderDivs $ cardSearchForm
	widgetVocabtrainCardChapterAddSearch chapterId cardSearchFormWidget cardSearchFormEnctype

widgetVocabtrainCardChapterAddSearch :: VocabChapterId -> GWidget App App () -> Enctype -> GWidget App App ()
widgetVocabtrainCardChapterAddSearch chapterId cardSearchFormWidget cardSearchFormEnctype = toWidget $(whamletFile "templates/vocabtrain/cardchapter_add_search.hamlet") 


icontains :: EntityField v Text -> Text -> Filter v
icontains field val = Filter field (Left $ Text.concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")

icontainsMaybe :: EntityField v (Maybe Text) -> Text -> Filter v
icontainsMaybe field val = Filter field (Left $ Just $ Text.concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")

getVocabtrainCardChapterAddR :: VocabChapterId -> Handler RepHtml
getVocabtrainCardChapterAddR chapterId = do
	msgShow <- getMessageRender
	maid <- maybeAuth 
	case maid of
		Nothing -> do
			permissionDenied $ msgShow MsgVocabManipulationPermissionDenied
		Just aid -> do
			chapter <- runDB $ get chapterId
			((result', cardSearchFormWidget), cardSearchFormEnctype) <- runFormGet $ renderDivs $ cardSearchForm
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
			setMessageI $ MsgCardChapterCreated 
			redirect $ VocabtrainChapterR chapterId -- TODO TranslationR translationId

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
	, requestLanguages :: [Text] -- TODO [TatoebaLanguage]
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
				VocabTranslationLanguage <-. ( map (read . Text.unpack) $ requestLanguages request)] []
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




getCardTypeText :: (AppMessage -> Text) -> CardType -> Text
getCardTypeText msgShow t = Text.intercalate " " $ map msgShow $ getCardTypeMessages t
getCardTypeMessages :: CardType -> [AppMessage]
getCardTypeMessages (CardTypeVerb secondary tertiary) = [toAppMessage CARDTYPE_VERB, toAppMessage secondary, toAppMessage tertiary]
getCardTypeMessages (CardTypeAdjective secondary tertiary) = [toAppMessage CARDTYPE_ADJECTIVE, toAppMessage secondary, toAppMessage tertiary]
getCardTypeMessages (CardTypeAdverb secondary tertiary) = [toAppMessage CARDTYPE_ADVERB, toAppMessage secondary, toAppMessage tertiary]
getCardTypeMessages (CardTypeAdposition) = [toAppMessage CARDTYPE_ADPOSITION]
getCardTypeMessages (CardTypeConjugation secondary) = [toAppMessage CARDTYPE_CONJUGATION, toAppMessage secondary]
getCardTypeMessages (CardTypeAbbreviation) = [toAppMessage CARDTYPE_ABBREVIATION]
getCardTypeMessages (CardTypeSaw) = [toAppMessage CARDTYPE_SAW]
getCardTypeMessages (CardTypeNoun secondary) = [toAppMessage CARDTYPE_NOUN, toAppMessage secondary ]
getCardTypeMessages _ = [toAppMessage CARDTYPE_UNKNOWN]

class VerbTypeMessage a where
	toAppMessage :: a -> AppMessage

instance VerbTypeMessage VerbType where
	toAppMessage CARDTYPE_VERB_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_TRANSITIVE = MsgCardTypeTransitive
	toAppMessage CARDTYPE_INTRANSITIVE = MsgCardTypeIntransitive
	toAppMessage CARDTYPE_REFLEXIVE = MsgCardTypeReflexive

instance VerbTypeMessage JapaneseVerbType where
	toAppMessage CARDTYPE_JAPANESEVERB_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_GODAN_DOUSHI = MsgCardTypeGodanDoushi
	toAppMessage CARDTYPE_ICHIDAN_DOUSHI = MsgCardTypeIchidanDoushi
	toAppMessage CARDTYPE_IRREGULAR_DOUSHI = MsgCardTypeIrregularDoushi

instance VerbTypeMessage AdjectiveType where
	toAppMessage CARDTYPE_ADJECTIVE_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_COMPARATIVE = MsgCardTypeComparative
	toAppMessage CARDTYPE_SUPERLATIVE = MsgCardTypeSuperlative

instance VerbTypeMessage JapaneseAdjectiveType where
	toAppMessage CARDTYPE_JAPANESEADJECTIVE_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_NCARDTYPE_ADJECTIVE = MsgCardTypeNaAdjective
	toAppMessage CARDTYPE_I_ADJECTIVE = MsgCardTypeIAdjective

instance VerbTypeMessage ConjugationType where
	toAppMessage CARDTYPE_CONJUGATION_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_PREPOSITION = MsgCardTypePreposition
	toAppMessage CARDTYPE_POSTPOSITION = MsgCardTypePostposition
	toAppMessage CARDTYPE_PARTICLE = MsgCardTypeParticle

instance VerbTypeMessage NounType where
	toAppMessage CARDTYPE_NOUN_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_FEMININE = MsgCardTypeFeminine
	toAppMessage CARDTYPE_MASCULINE = MsgCardTypeMasculine
	toAppMessage CARDTYPE_NEUTER = MsgCardTypeNeuter
	toAppMessage CARDTYPE_FEMININE_PLURAL = MsgCardTypeFemininePlural
	toAppMessage CARDTYPE_MASCULINE_PLURAL = MsgCardTypeMasculinePlural
	toAppMessage CARDTYPE_NEUTER_PLURAL = MsgCardTypeNeuterPlural

instance VerbTypeMessage CardTypePrimary where
	toAppMessage CARDTYPE_UNKNOWN  = MsgCardTypeUnknown
	toAppMessage CARDTYPE_VERB = MsgCardTypeVerb
	toAppMessage CARDTYPE_ADJECTIVE = MsgCardTypeAdjective
	toAppMessage CARDTYPE_ADVERB = MsgCardTypeAdverb
	toAppMessage CARDTYPE_ADPOSITION = MsgCardTypeAdposition
	toAppMessage CARDTYPE_CONJUGATION = MsgCardTypeConjugation
	toAppMessage CARDTYPE_ABBREVIATION = MsgCardTypeAbbreviation
	toAppMessage CARDTYPE_SAW = MsgCardTypeSaw
	toAppMessage CARDTYPE_NOUN = MsgCardTypeNoun

