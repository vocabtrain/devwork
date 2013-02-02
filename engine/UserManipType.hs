module UserManipType where
import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Store
import Text.Read
import Text.ParserCombinators.ReadP hiding (choice)
import ShowText

data UserManipType = USERMANIP_UPDATE | USERMANIP_INSERT | USERMANIP_PUT | USERMANIP_REMOVE
	deriving(Eq,Show)
instance ShowText UserManipType where
	showText USERMANIP_UPDATE = "u"
	showText USERMANIP_INSERT = "i"
	showText USERMANIP_PUT = "p"
	showText USERMANIP_REMOVE = "r"
{-
instance FromText UserManipType where 
	fromText "u" = USERMANIP_UPDATE
	fromText "d" = USERMANIP_DELETE
	fromText "i" = USERMANIP_INSERT
	fromText _ = ""-}
instance Read UserManipType where
	readPrec = choice $ strValMap 
		[ ("u",USERMANIP_UPDATE)
		, ("i",USERMANIP_INSERT)
		, ("p",USERMANIP_PUT)
		, ("r",USERMANIP_REMOVE)
		]
		where
			strValMap :: [(String, UserManipType)] -> [ReadPrec UserManipType]
			strValMap = map (\(x, y) -> lift $ string x >> return y)

instance PersistField UserManipType where
	toPersistValue = toPersistValue . showText 
	fromPersistValue value = either Left (Right . read . Text.unpack) (fromPersistValue value :: (Either Text Text))
	sqlType = sqlType . showText

