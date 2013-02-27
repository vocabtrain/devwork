{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tatoeba where

import Import
import GlobalLayout
import TatoebaLanguageWidget
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

import Data.String

data TatoebaSentence = TatoebaSentence {
	tatoebaSentenceId :: Int,
	tatoebaSentenceLanguage :: TatoebaLanguage,
	tatoebaSentenceText :: Text
	}
	deriving Show

data TatoebaRelation = TatoebaRelation TatoebaSentence [TatoebaSentence] deriving Show

tatoebaSentenceFromQuery :: [PersistValue] -> TatoebaSentence
tatoebaSentenceFromQuery values = TatoebaSentence
	(either (\_ -> -1) Prelude.id $ fromPersistValue (values !! 0)) 
	(either (\_ -> LANG_UND) Prelude.id $ fromPersistValue (values !! 1)) 
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
		NodeElement $ Element "mainsentence" (Map.fromList [("id", Text.pack . show $ tatoebaSentenceId sentence), ("language", Text.pack $ show $ tatoebaSentenceLanguage sentence)  ]) [ NodeContent $ tatoebaSentenceText sentence]
		]  ++ map sentenceToXml translations)

sentenceToXml :: TatoebaSentence -> Node
sentenceToXml sentence = NodeElement $ Element "sentence" (Map.fromList [("id", Text.pack . show $ tatoebaSentenceId sentence), ("language", Text.pack $ show $ tatoebaSentenceLanguage sentence)  ]) [ NodeContent $ tatoebaSentenceText sentence]



getVocabtrainMobileVeecheckR :: Handler RepXml
getVocabtrainMobileVeecheckR = do
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) (Element nameVersions Map.empty $ map addVeecheckRule veecheckActions) []
	return $ RepXml content
	where
		addVeecheckRule :: (Text, Text) -> Node 
		addVeecheckRule (version,action) = NodeElement $ Element nameVersion (Map.fromList [("versionName", version)]) [ NodeElement $ Element nameIntent (Map.fromList [("action", action)]) [] ] 
		veecheckNamespace :: String
		veecheckNamespace = "{http://www.tomgibara.com/android/veecheck/SCHEMA.1}"
		nameVersions :: Name
		nameVersions = fromString $ veecheckNamespace ++ "versions"
		nameVersion :: Name
		nameVersion = fromString $ veecheckNamespace ++ "version"
		nameIntent :: Name
		nameIntent = fromString $ veecheckNamespace ++  "intent"
		veecheckActions :: [(Text, Text)]
		veecheckActions = [ ("1.0.0", "com.example.app.ACTION_UPGRADE") ]

data RepHtmlXml = RepHtmlXml Content Content
instance HasReps RepHtmlXml where
	chooseRep (RepHtmlXml html xml) = chooseRep
		[ (typeXml, xml)
		, (typeHtml, html)
		]

-- QueryR

getTatoebaQueryR :: TatoebaLanguage -> Text -> Handler RepHtmlXml
getTatoebaQueryR language queryString = do
	relations <- tatoebaQuerySentences language queryString
	RepHtml html <- tatoebaRelationsToHtml relations
	RepXml  xml  <- tatoebaRelationsToXml  relations
	return $ RepHtmlXml html xml

tatoebaRelationsToHtml :: [TatoebaRelation] -> Handler RepHtml
tatoebaRelationsToHtml relations = do
	globalLayout' "search" $(widgetFile "tatoeba/search")

tatoebaRelationsToXml :: [TatoebaRelation] -> Handler RepXml
tatoebaRelationsToXml relations = do
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) (relationsToXml relations) []
	return $ RepXml content

tatoebaQuerySentences ::  TatoebaLanguage -> Text -> GHandler App App [TatoebaRelation]
tatoebaQuerySentences language queryString = do
	sphinxResult <- liftIO $ querySphinx (Text.pack . show $ language) queryString
	fmap catMaybes $ sequence $ map (\sentence_id -> do
		textResult <- runDB $ C.runResourceT $ withStmt
			"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences WHERE sentence_id = ?"
			[toPersistValue sentence_id] C.$$ CL.consume
		tatoebaRelationFromQuery $ textResult !! 0
		) sphinxResult

-- tatoebaLanguageR
getTatoebaQueryLanguageR :: TatoebaLanguage -> Handler RepHtmlXml
getTatoebaQueryLanguageR language = do
	mrelation <- tatoebaQueryRandomSentenceIn language
	RepHtml html <- tatoebaRelationToHtml mrelation
	RepXml  xml  <- tatoebaRelationToXml  mrelation
	return $ RepHtmlXml html xml


tatoebaRelationToHtml :: Maybe TatoebaRelation -> Handler RepHtml
tatoebaRelationToHtml mrelation = do
	case mrelation of
		Just relation -> tatoebaRelationsToHtml [relation]
		Nothing -> notFound

tatoebaRelationToXml :: Maybe TatoebaRelation -> Handler RepXml
tatoebaRelationToXml mrelation = return $ RepXml $ toContent $ renderText def $ Document (Prologue [] Nothing []) (getContent mrelation) []
	where 
		getContent :: Maybe TatoebaRelation -> Element
		getContent Nothing = Element "relation" Map.empty []
		getContent (Just relation) = relationToXml relation
	
tatoebaQueryRandomSentenceIn :: TatoebaLanguage -> GHandler App App (Maybe TatoebaRelation)
tatoebaQueryRandomSentenceIn language = do
	textResult <- runDB $ C.runResourceT $ withStmt
		"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences WHERE sentence_language = ? ORDER BY RANDOM() LIMIT 1;" 
		[toPersistValue language] C.$$ CL.consume
	tatoebaRelationFromQuery $ textResult !! 0

-- TatoebaQueryRandomR

getTatoebaQueryRandomR :: Handler RepHtmlXml
getTatoebaQueryRandomR = do
	mrelation <- queryRandomSentence
	RepHtml html <- tatoebaRelationToHtml mrelation
	RepXml  xml  <- tatoebaRelationToXml  mrelation
	return $ RepHtmlXml html xml

queryRandomSentence :: GHandler App App (Maybe TatoebaRelation)
queryRandomSentence = do
	textResult <- runDB $ C.runResourceT $ withStmt
		"SELECT sentence_id, sentence_language, sentence_text FROM tatoeba_sentences ORDER BY RANDOM() LIMIT 1;" 
		[] C.$$ CL.consume
	tatoebaRelationFromQuery $ textResult !! 0

-- TatoebaLanguagesR

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

