{-# LANGUAGE FlexibleInstances #-}
module UserManipLog where

import Data.Time
import UserManipType
import Data.Text (Text)
import Import
import ToAppMessage

data UserManipLog = UserManipLog 
	{ userManipUserNick :: Maybe Text
	, userManipType :: UserManipType
	, userManipTimestamp :: UTCTime
	, userManipContent :: Text
	}
class ToUserManipLog a where
	toUserManipLog :: a -> GHandler App App UserManipLog

getUserNick :: UserId -> GHandler App App (Maybe Text)
getUserNick userId = do
	muser <- runDB $ get userId
	case muser of
		Just user -> return $ userNick user
		Nothing -> return Nothing

instance ToUserManipLog VocabBookManip where
	toUserManipLog log = do
		nick <- getUserNick $ vocabBookManipUserId log
		return UserManipLog
			{ userManipUserNick = nick
			, userManipType = vocabBookManipType log
			, userManipTimestamp = vocabBookManipTimestamp log
			, userManipContent = vocabBookManipContent log
			}
getUserManipTypeIcon :: UserManipType -> Text
getUserManipTypeIcon USERMANIP_INSERT = "icon-file"
getUserManipTypeIcon USERMANIP_UPDATE = "icon-refresh"
getUserManipTypeIcon USERMANIP_DELETE = "icon-trash"

userManipTypeWidget :: UserManipType -> GWidget App App()
userManipTypeWidget t = do
	msgShow <- lift $ getMessageRender
	toWidget $ [hamlet|<i .#{getUserManipTypeIcon t} title=#{msgShow $ toAppMessage t} data-placement="bottom" rel="tooltip"> |]

