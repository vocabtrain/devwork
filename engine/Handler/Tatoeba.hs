{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tatoeba where

import Import
import Data.Maybe
import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Map as Map
import Text.XML

import Sphinx

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.Store
import Database.Persist.GenericSql.Raw (withStmt)

data TatoebaSentence = TatoebaSentence {
	sentenceId :: Int,
	sentenceLanguage :: Text,
	sentenceText :: Text
	}
	deriving Show

data TatoebaRelation = TatoebaRelation TatoebaSentence [TatoebaSentence] deriving Show

tatoebaSentenceFromQuery :: [PersistValue] -> TatoebaSentence
tatoebaSentenceFromQuery values = TatoebaSentence
	(either (\_ -> -1) Prelude.id $ fromPersistValue (values !! 0)) 
	(either (\_ -> ""::Text) Prelude.id $ fromPersistValue (values !! 1)) 
	(either (\_ -> ""::Text) Prelude.id $ fromPersistValue (values !! 2)) 
	

tatoebaTranslationsFromQuery :: Int -> GHandler App App ([TatoebaSentence])
tatoebaTranslationsFromQuery sentence_id = do
	translationResult <- runDB $ C.runResourceT $ withStmt
		"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences JOIN tatoeba_links ON sentence_id = link_translation_id WHERE link_sentence_id = ?"
		[toPersistValue sentence_id] C.$$ CL.consume
	return $ map (\translation -> tatoebaSentenceFromQuery translation) translationResult

tatoebaRelationFromQuery :: [PersistValue] -> GHandler App App (Maybe TatoebaRelation)
tatoebaRelationFromQuery mainSentenceResult = do
	if null mainSentenceResult
		then do
			return Nothing
		else do
			let sentence_id = either (\_ -> -1) Prelude.id $ fromPersistValue (mainSentenceResult !! 0)
			translations <- tatoebaTranslationsFromQuery sentence_id
			return $ Just $ TatoebaRelation (tatoebaSentenceFromQuery $ mainSentenceResult) translations

relationsToXml :: [TatoebaRelation] -> Element
relationsToXml relations = Element "relations" Map.empty $ map (NodeElement . relationToXml) relations

relationToXml :: TatoebaRelation -> Element
relationToXml (TatoebaRelation sentence translations) = 
	Element "relation" Map.empty ([ 
		NodeElement $ Element "mainsentence" (Map.fromList [("id", Text.pack . show $ sentenceId sentence), ("language", sentenceLanguage sentence)  ]) [ NodeContent $ sentenceText sentence]
		]  ++ map sentenceToXml translations)

sentenceToXml :: TatoebaSentence -> Node
sentenceToXml sentence = NodeElement $ Element "sentence" (Map.fromList [("id", Text.pack . show $ sentenceId sentence), ("language", sentenceLanguage sentence)  ]) [ NodeContent $ sentenceText sentence]


getTatoebaQueryR :: TatoebaLanguage -> Text -> Handler RepXml
getTatoebaQueryR language queryString = do
--	liftIO $ putStr (Text.unpack queryString)
	relations <- querySentences 
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) (relationsToXml relations) []
	return $ RepXml content
	where
		querySentences :: GHandler App App [TatoebaRelation]
		querySentences = do
			sphinxResult <- liftIO $ querySphinx (Text.pack . show $ language) queryString
			fmap catMaybes $ sequence $ map (\sentence_id -> do
				textResult <- runDB $ C.runResourceT $ withStmt
					"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences WHERE sentence_id = ?"
					[toPersistValue sentence_id] C.$$ CL.consume
				tatoebaRelationFromQuery $ textResult !! 0
				) sphinxResult

handleTatoebaQueryLanguageR :: TatoebaLanguage -> Handler RepXml
handleTatoebaQueryLanguageR language = do
	relation <- queryRandomSentenceIn
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) 
		(if isJust relation then relationToXml $ fromJust relation else Element "relation" Map.empty []) []
	return $ RepXml content
	where
		queryRandomSentenceIn :: GHandler App App (Maybe TatoebaRelation)
		queryRandomSentenceIn = do
			textResult <- runDB $ C.runResourceT $ withStmt
				"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences WHERE sentence_language = ? ORDER BY RANDOM() LIMIT 1;" 
				[toPersistValue language] C.$$ CL.consume
			tatoebaRelationFromQuery $ textResult !! 0

getTatoebaQueryRandomR :: Handler RepXml
getTatoebaQueryRandomR = do
	relation <- queryRandomSentence 
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) 
		(if isJust relation then relationToXml $ fromJust relation else Element "relation" Map.empty []) []
	return $ RepXml content
	where
		queryRandomSentence :: GHandler App App (Maybe TatoebaRelation)
		queryRandomSentence = do
			textResult <- runDB $ C.runResourceT $ withStmt
				"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences ORDER BY RANDOM() LIMIT 1;" 
				[] C.$$ CL.consume
			tatoebaRelationFromQuery $ textResult !! 0

getTatoebaLanguagesR :: Handler RepXml
getTatoebaLanguagesR = do
	textResult <- runDB $ C.runResourceT $ withStmt
		"SELECT sentence_language from tatoeba_sentences group by sentence_language;"
		[] C.$$ CL.consume
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) 
		(Element "languages" Map.empty $ catMaybes $ map 
			(\languageResult -> either 
				(\_ -> Nothing) 
				(\language -> Just $ NodeElement $ Element "language" Map.empty [NodeContent language])
				$ fromPersistValue (languageResult !! 0)
			) textResult
		)
		[]
	return $ RepXml content

