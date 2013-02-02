module ToAppMessage where

import Import
import CardType
import UserManipType
import qualified Data.Text as Text


class ToAppMessage a where
	toAppMessage :: a -> AppMessage

instance ToAppMessage VerbType where
	toAppMessage CARDTYPE_VERB_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_TRANSITIVE = MsgCardTypeTransitive
	toAppMessage CARDTYPE_INTRANSITIVE = MsgCardTypeIntransitive
	toAppMessage CARDTYPE_REFLEXIVE = MsgCardTypeReflexive

instance ToAppMessage JapaneseVerbType where
	toAppMessage CARDTYPE_JAPANESEVERB_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_GODAN_DOUSHI = MsgCardTypeGodanDoushi
	toAppMessage CARDTYPE_ICHIDAN_DOUSHI = MsgCardTypeIchidanDoushi
	toAppMessage CARDTYPE_IRREGULAR_DOUSHI = MsgCardTypeIrregularDoushi

instance ToAppMessage AdjectiveType where
	toAppMessage CARDTYPE_ADJECTIVE_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_COMPARATIVE = MsgCardTypeComparative
	toAppMessage CARDTYPE_SUPERLATIVE = MsgCardTypeSuperlative

instance ToAppMessage JapaneseAdjectiveType where
	toAppMessage CARDTYPE_JAPANESEADJECTIVE_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_NCARDTYPE_ADJECTIVE = MsgCardTypeNaAdjective
	toAppMessage CARDTYPE_I_ADJECTIVE = MsgCardTypeIAdjective

instance ToAppMessage ConjugationType where
	toAppMessage CARDTYPE_CONJUGATION_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_PREPOSITION = MsgCardTypePreposition
	toAppMessage CARDTYPE_POSTPOSITION = MsgCardTypePostposition
	toAppMessage CARDTYPE_PARTICLE = MsgCardTypeParticle

instance ToAppMessage NounType where
	toAppMessage CARDTYPE_NOUN_NONE = MsgCardTypeNone
	toAppMessage CARDTYPE_FEMININE = MsgCardTypeFeminine
	toAppMessage CARDTYPE_MASCULINE = MsgCardTypeMasculine
	toAppMessage CARDTYPE_NEUTER = MsgCardTypeNeuter
	toAppMessage CARDTYPE_FEMININE_PLURAL = MsgCardTypeFemininePlural
	toAppMessage CARDTYPE_MASCULINE_PLURAL = MsgCardTypeMasculinePlural
	toAppMessage CARDTYPE_NEUTER_PLURAL = MsgCardTypeNeuterPlural

instance ToAppMessage CardTypePrimary where
	toAppMessage CARDTYPE_UNKNOWN  = MsgCardTypeUnknown
	toAppMessage CARDTYPE_VERB = MsgCardTypeVerb
	toAppMessage CARDTYPE_ADJECTIVE = MsgCardTypeAdjective
	toAppMessage CARDTYPE_ADVERB = MsgCardTypeAdverb
	toAppMessage CARDTYPE_ADPOSITION = MsgCardTypeAdposition
	toAppMessage CARDTYPE_CONJUGATION = MsgCardTypeConjugation
	toAppMessage CARDTYPE_ABBREVIATION = MsgCardTypeAbbreviation
	toAppMessage CARDTYPE_SAW = MsgCardTypeSaw
	toAppMessage CARDTYPE_NOUN = MsgCardTypeNoun


instance ToAppMessage UserManipType where
	toAppMessage USERMANIP_INSERT = MsgUserManipInsert
	toAppMessage USERMANIP_UPDATE = MsgUserManipUpdate
	toAppMessage USERMANIP_PUT = MsgUserManipPut
	toAppMessage USERMANIP_REMOVE = MsgUserManipRemove

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
