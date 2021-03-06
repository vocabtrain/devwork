{-# LANGUAGE FlexibleInstances  #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text, pack)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Generated
import CardType
import UserManipType 
import Data.Typeable (Typeable)

{-
data CardTypePrimary = CARDTYPE_VERB | CARDTYPE_NOUN deriving(Enum,Show,Eq,Read, Bounded)
data CardTypeSecondary = CARDTYPE_TRANSITIVE | CARDTYPE_INTRANSITIVE deriving(Enum,Show,Eq,Read, Bounded)
data CardTypeTertiary = CARDTYPE_GROUPI | CARDTYPE_GROUPII deriving(Enum,Show,Eq,Read, Bounded)

data CardType = CardType {
	  cardTypePrimary :: CardTypePrimary
	, cardTypeSecondary :: CardTypeSecondary
	, cardTypeTertiary :: CardTypeTertiary
	} deriving(Show,Eq,Read)

instance Enum CardType where
	toEnum i = CardType {
		  cardTypePrimary   = toEnum $ i `div` 100
		, cardTypeSecondary = toEnum $ div (i `mod` 100) 10
		, cardTypeTertiary  = toEnum $ i `mod` 10
		}
	fromEnum t = (fromEnum $ cardTypeTertiary t) + ((*) 10 $ fromEnum $ cardTypeSecondary t) + ((*) 100 $ fromEnum $ cardTypeTertiary t)

instance PersistField CardType where
	toPersistValue = toPersistValue . fromEnum
	fromPersistValue value = either Left (Right . toEnum) (fromPersistValue value :: (Either Text Int))
	sqlType = sqlType . fromEnum 
-}

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
{-
share [mkPersist sqlSettings, mkMigrate "migrateCore", mkDeleteCascade sqlSettings]
    $(persistFileWith lowerCaseSettings "config/models")

share [mkPersist sqlSettings, mkMigrate "migrateVocabtrain", mkDeleteCascade sqlSettings]
    $(persistFileWith lowerCaseSettings "config/vocabtrainmodels")

share [mkPersist sqlSettings, mkMigrate "migrateVocabtrainServer", mkDeleteCascade sqlSettings]
    $(persistFileWith lowerCaseSettings "config/vocabtrainservermodels")
-}

share [mkPersist sqlOnlySettings, mkDeleteCascade sqlOnlySettings] $
	 $(persistFileWith lowerCaseSettings "config/models") 
	 ++ $(persistFileWith lowerCaseSettings "config/vocabtrainmodels")
	 ++ $(persistFileWith lowerCaseSettings "config/vocabtrainservermodels")
	 ++ $(persistFileWith lowerCaseSettings "config/vocabtrainmobilemodels")
	 ++ $(persistFileWith lowerCaseSettings "config/tatoebamodels")

share [mkMigrate "migrateVocabtrainMobile"] $
	$(persistFileWith lowerCaseSettings "config/vocabtrainmodels")
	++ $(persistFileWith lowerCaseSettings "config/vocabtrainmobilemodels") 

share [mkMigrate "migrateServer"] $
	 $(persistFileWith lowerCaseSettings "config/models") 
	 ++ $(persistFileWith lowerCaseSettings "config/vocabtrainmodels") 
	 ++ $(persistFileWith lowerCaseSettings "config/vocabtrainservermodels")
	 ++ $(persistFileWith lowerCaseSettings "config/tatoebamodels")

--instance ToMessage VocabBookId where
--    toMessage i = Data.Text.pack $ show i
instance ToMessage Int where
    toMessage i = Data.Text.pack $ show i
