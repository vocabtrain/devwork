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

import BarefootSQL

import Data.String
import Control.Monad

data TatoebaRelation = TatoebaRelation (Entity TatoebaSentence) [Entity TatoebaSentence] deriving Show


--TODO : Catch all Translations with IN(...) instead of catching for each sentence the translations

tatoebaRelationFromQuery :: Maybe (Entity TatoebaSentence) -> Handler (Maybe TatoebaRelation)
tatoebaRelationFromQuery mainSentenceResult = case mainSentenceResult of
	Nothing -> return Nothing
	Just mainSentence -> do
		translations <- runDB . getTatoebaTranslationsSQL $ entityKey mainSentence
		return .  Just $ TatoebaRelation mainSentence translations

relationsToXml :: [TatoebaRelation] -> Element
relationsToXml relations = Element "relations" Map.empty $ map (NodeElement . relationToXml) relations


relationToXml :: TatoebaRelation -> Element
relationToXml (TatoebaRelation sentence translations) = 
	Element "relation" Map.empty ([ 
		sentenceToXml  "mainsentence" sentence
		]  ++ map (sentenceToXml "sentence") translations)

sentenceToXml :: Name -> Entity TatoebaSentence -> Node
sentenceToXml name sentence = NodeElement $ Element name
	(Map.fromList 
		[("id" , sentence_id)
		,("language", Text.pack $ show $ tatoebaSentenceLanguage $ entityVal sentence)  ]) 
	[ NodeContent $ tatoebaSentenceText $ entityVal sentence]
	where sentence_id =
		case fromPersistValue . unKey . entityKey $ sentence of
			Left _ -> ""
			Right val -> val


-- TODO: This here is lost code?
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


-- QueryR

getTatoebaQueryR :: TatoebaLanguage -> Text -> Handler TypedContent
getTatoebaQueryR language queryString = do
	relations <- tatoebaQuerySentences language queryString
	selectRep $ do
		provideRep $ tatoebaRelationsToHtml relations
		provideRep $ tatoebaRelationsToXml  relations

tatoebaRelationsToHtml :: [TatoebaRelation] -> Handler Html
tatoebaRelationsToHtml relations = do
	globalLayout' "search" $(widgetFile "tatoeba/search")

tatoebaRelationsToXml :: [TatoebaRelation] -> Handler RepXml
tatoebaRelationsToXml relations = do
	let content = toContent $ renderText def $ Document (Prologue [] Nothing []) (relationsToXml relations) []
	return $ RepXml content

tatoebaQuerySentences ::  TatoebaLanguage -> Text -> Handler [TatoebaRelation]
tatoebaQuerySentences language queryString = do
	sphinxResult <- liftIO $ querySphinx (Text.pack . show $ language) queryString
	relations <- forM sphinxResult $ \sentence_id -> do
		let key = Key $ PersistInt64 sentence_id :: TatoebaSentenceId
		textResult <- runDB $ get key 
		case textResult of
			Just sentence -> tatoebaRelationFromQuery $ Just $ Entity { entityKey = key, entityVal = sentence }
			Nothing -> return $ Nothing
	return $ catMaybes relations


-- tatoebaLanguageR
getTatoebaQueryLanguageR :: TatoebaLanguage -> Handler TypedContent
getTatoebaQueryLanguageR language = do
	mrelation <- tatoebaQueryRandomSentenceIn language
	selectRep $ do
		provideRep $ tatoebaRelationToHtml mrelation
		provideRep $ tatoebaRelationToXml  mrelation


tatoebaRelationToHtml :: Maybe TatoebaRelation -> Handler Html
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
	
tatoebaQueryRandomSentenceIn :: TatoebaLanguage -> Handler (Maybe TatoebaRelation)
tatoebaQueryRandomSentenceIn language = (runDB $ getTatoebaRandomSentenceWithLanguageSQL language) >>= tatoebaRelationFromQuery

-- TatoebaQueryRandomR

getTatoebaQueryRandomR :: Handler TypedContent
getTatoebaQueryRandomR = do
	mrelation <- queryRandomSentence
	selectRep $ do
		provideRep $ tatoebaRelationToHtml mrelation
		provideRep $ tatoebaRelationToXml  mrelation

queryRandomSentence :: Handler (Maybe TatoebaRelation)
queryRandomSentence = (runDB $ getTatoebaRandomSentenceSQL) >>= tatoebaRelationFromQuery

-- TatoebaLanguagesR

getTatoebaLanguagesR :: Handler RepXml
getTatoebaLanguagesR = do
	runDB getTatoebaLanguagesSQL >>= (\langResult -> do
		let body = Element "languages" Map.empty $ map (\lang -> NodeElement $ Element "language" Map.empty [NodeContent $ Text.pack $ show lang ]) langResult 
		return $ RepXml $ toContent $ renderText def $ Document (Prologue [] Nothing []) body []
		)
