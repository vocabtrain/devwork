{-# LANGUAGE FlexibleInstances #-}
module CardType where
import Prelude
import Data.Text (Text)
import Database.Persist
import qualified Data.Aeson as JS
import Control.Applicative (pure)
import Database.Persist.Sql



data VerbType = CARDTYPE_VERB_NONE | CARDTYPE_TRANSITIVE | CARDTYPE_INTRANSITIVE | CARDTYPE_REFLEXIVE 
	deriving(Enum,Show,Eq,Read, Bounded)
data JapaneseVerbType = CARDTYPE_JAPANESEVERB_NONE | CARDTYPE_GODAN_DOUSHI | CARDTYPE_ICHIDAN_DOUSHI | CARDTYPE_IRREGULAR_DOUSHI
	deriving(Enum,Show,Eq,Read, Bounded)
data AdjectiveType = CARDTYPE_ADJECTIVE_NONE | CARDTYPE_COMPARATIVE | CARDTYPE_SUPERLATIVE 
	deriving(Enum,Show,Eq,Read, Bounded)
data JapaneseAdjectiveType = CARDTYPE_JAPANESEADJECTIVE_NONE | CARDTYPE_NCARDTYPE_ADJECTIVE | CARDTYPE_I_ADJECTIVE 
	deriving(Enum,Show,Eq,Read, Bounded)
data ConjugationType = CARDTYPE_CONJUGATION_NONE | CARDTYPE_PREPOSITION | CARDTYPE_POSTPOSITION | CARDTYPE_PARTICLE 
	deriving(Enum,Show,Eq,Read, Bounded)
data NounType = CARDTYPE_NOUN_NONE | CARDTYPE_FEMININE | CARDTYPE_MASCULINE | CARDTYPE_NEUTER | CARDTYPE_FEMININE_PLURAL | CARDTYPE_MASCULINE_PLURAL | CARDTYPE_NEUTER_PLURAL
	deriving(Enum,Show,Eq,Read, Bounded)

data CardTypePrimary = CARDTYPE_UNKNOWN | CARDTYPE_VERB | CARDTYPE_ADJECTIVE | CARDTYPE_ADVERB | CARDTYPE_ADPOSITION | CARDTYPE_CONJUGATION | CARDTYPE_ABBREVIATION | CARDTYPE_SAW | CARDTYPE_NOUN
	deriving(Enum,Show,Eq,Read, Bounded)

getCardTypeBounds :: CardTypePrimary -> (Int, Int)
getCardTypeBounds CARDTYPE_VERB = (fromEnum (maxBound::VerbType), fromEnum (maxBound::JapaneseVerbType))
getCardTypeBounds CARDTYPE_ADJECTIVE = (fromEnum (maxBound::AdjectiveType), fromEnum (maxBound::JapaneseAdjectiveType))
getCardTypeBounds CARDTYPE_ADVERB = (fromEnum (maxBound::AdjectiveType), fromEnum (maxBound::JapaneseAdjectiveType))
getCardTypeBounds CARDTYPE_ADPOSITION = (0,0)
getCardTypeBounds CARDTYPE_CONJUGATION = (fromEnum (maxBound::ConjugationType), 0)
getCardTypeBounds CARDTYPE_ABBREVIATION = (0,0)
getCardTypeBounds CARDTYPE_SAW = (0,0)
getCardTypeBounds CARDTYPE_NOUN = (fromEnum (maxBound::NounType), 0)
getCardTypeBounds _ = (0,0)

data CardType = CardTypeVerb VerbType JapaneseVerbType |
	CardTypeAdjective AdjectiveType JapaneseAdjectiveType |
	CardTypeAdverb AdjectiveType JapaneseAdjectiveType |
	CardTypeAdposition |
	CardTypeConjugation ConjugationType |
	CardTypeAbbreviation |
	CardTypeSaw |
	CardTypeNoun NounType |
	CardTypeUnknown
	deriving(Show,Eq)

instance Enum CardType where
	toEnum i = case toEnum primary of 
		CARDTYPE_VERB -> CardTypeVerb (toEnum secondary) (toEnum tertiary)
		CARDTYPE_ADJECTIVE -> CardTypeAdjective (toEnum secondary) (toEnum tertiary)
		CARDTYPE_ADVERB -> CardTypeAdverb (toEnum secondary) (toEnum tertiary)
		CARDTYPE_ADPOSITION -> CardTypeAdposition
		CARDTYPE_CONJUGATION -> CardTypeConjugation (toEnum secondary)
		CARDTYPE_ABBREVIATION -> CardTypeAbbreviation
		CARDTYPE_SAW -> CardTypeSaw
		CARDTYPE_NOUN -> CardTypeNoun (toEnum secondary)
		CARDTYPE_UNKNOWN -> CardTypeUnknown
		where 
			primary = i `mod` 100
			tertiary = i `div` 1000
			secondary = (i `div` 100) `mod` 10
	fromEnum (CardTypeVerb secondary tertiary) = calcCardType CARDTYPE_VERB (fromEnum secondary) (fromEnum tertiary)
	fromEnum (CardTypeAdjective secondary tertiary) = calcCardType CARDTYPE_ADJECTIVE (fromEnum secondary) (fromEnum tertiary)
	fromEnum (CardTypeAdverb secondary tertiary) = calcCardType CARDTYPE_ADVERB (fromEnum secondary) (fromEnum tertiary)
	fromEnum (CardTypeAdposition) = calcCardType'' CARDTYPE_ADPOSITION
	fromEnum (CardTypeConjugation secondary) = calcCardType' CARDTYPE_CONJUGATION (fromEnum secondary)
	fromEnum (CardTypeAbbreviation) = calcCardType'' CARDTYPE_ABBREVIATION
	fromEnum (CardTypeSaw) = calcCardType'' CARDTYPE_SAW
	fromEnum (CardTypeNoun secondary) = calcCardType' CARDTYPE_NOUN (fromEnum secondary) 
	fromEnum _ = calcCardType'' CARDTYPE_UNKNOWN

calcCardType :: CardTypePrimary -> Int -> Int -> Int
calcCardType pri secondary tertiary = tertiary * 1000 + calcCardType' pri secondary
calcCardType' :: CardTypePrimary -> Int -> Int 
calcCardType' pri secondary  = 100 * secondary + calcCardType'' pri
calcCardType'' :: CardTypePrimary -> Int 
calcCardType'' pri = fromEnum pri
calcCardTypeList :: [Int] -> Int 
calcCardTypeList lst = foldl (\x y -> x*10+y) 0 lst


getAllCardTypes :: [CardType]
getAllCardTypes = map (\l -> toEnum $ calcCardTypeList l) $ concat $ 
	map (\pri -> map (\l -> l ++ (priToList pri) ) $ 
		sequence $ map (\x -> [0..x]) $ getCardTypeBounds' pri) ([minBound..maxBound] :: [CardTypePrimary]) 
	:: [CardType]
	where
		priToList :: CardTypePrimary -> [Int]
		priToList = priToList' . fromEnum
		priToList' :: Int -> [Int]
		priToList' i
			| i < 9 = [0,i]
			| otherwise = [i]

{-
main = do
	print ( 
		map (\l -> toEnum $ calcCardTypeList l) $ concat $ map (\pri -> map (\l -> (fromEnum pri) : l) $ sequence $ map (\x -> [0..x]) $ getCardTypeBounds' pri) ([minBound..maxBound] :: [CardTypePrimary]) :: [CardType]
		)
-}
getCardTypeBounds' :: CardTypePrimary -> [Int]
getCardTypeBounds' t = [snd v, fst v]
	where v = getCardTypeBounds t

instance PersistField CardType where
	toPersistValue = toPersistValue . fromEnum
	fromPersistValue value = either Left (Right . toEnum) (fromPersistValue value :: (Either Text Int))
instance PersistFieldSql CardType where
	--sqlType = sqlType . fromEnum 
	sqlType _ = SqlInt32

instance JS.FromJSON CardType where
	parseJSON = JS.withNumber "Integral" (\t -> pure $ toEnum $ floor t) 
