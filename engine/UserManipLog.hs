{-# LANGUAGE FlexibleInstances #-}
module UserManipLog where

import Data.Time
import UserManipType
--import Data.Text (Text)
import Import
import ToAppMessage

data UserManipLog = UserManipLog 
	{ userManipUserNick :: Maybe Text
	, userManipType :: UserManipType
	, userManipTimestamp :: UTCTime
	, userManipContent :: Text
	}
class ToUserManipLog a where
	toUserManipLog :: a -> HandlerT App IO UserManipLog

getUserNick :: UserId -> HandlerT App IO (Maybe Text)
getUserNick userId = do
	muser <- runDB $ get userId
	case muser of
		Just user -> return $ userNick user
		Nothing -> return Nothing

instance ToUserManipLog VocabBookManip where
	toUserManipLog manipLog= do
		nick <- getUserNick $ vocabBookManipUserId manipLog 
		return UserManipLog
			{ userManipUserNick = nick
			, userManipType = vocabBookManipType manipLog
			, userManipTimestamp = vocabBookManipTimestamp manipLog
			, userManipContent = vocabBookManipContent manipLog
			}

instance ToUserManipLog VocabChapterManip where
	toUserManipLog manipLog= do
		nick <- getUserNick $ vocabChapterManipUserId manipLog 
		return UserManipLog
			{ userManipUserNick = nick
			, userManipType = vocabChapterManipType manipLog
			, userManipTimestamp = vocabChapterManipTimestamp manipLog
			, userManipContent = vocabChapterManipContent manipLog
			}
instance ToUserManipLog VocabTranslationManip where
	toUserManipLog manipLog= do
		nick <- getUserNick $ vocabTranslationManipUserId manipLog 
		return UserManipLog
			{ userManipUserNick = nick
			, userManipType = vocabTranslationManipType manipLog
			, userManipTimestamp = vocabTranslationManipTimestamp manipLog
			, userManipContent = vocabTranslationManipContent manipLog
			}
instance ToUserManipLog VocabCardManip where
	toUserManipLog manipLog= do
		nick <- getUserNick $ vocabCardManipUserId manipLog 
		return UserManipLog
			{ userManipUserNick = nick
			, userManipType = vocabCardManipType manipLog
			, userManipTimestamp = vocabCardManipTimestamp manipLog
			, userManipContent = vocabCardManipContent manipLog
			}



getUserManipTypeIcon :: UserManipType -> Text
getUserManipTypeIcon USERMANIP_INSERT = "icon-file"
getUserManipTypeIcon USERMANIP_UPDATE = "icon-refresh"
getUserManipTypeIcon USERMANIP_PUT = "icon-star"
getUserManipTypeIcon USERMANIP_REMOVE = "icon-trash"


--userManipTypeWidget :: (MonadTrans t, ToWidget (HandlerSite (t m)) MonadWidget (t m), MonadHandler m, RenderMessage (HandlerSite m) AppMessage) => UserManipType -> t m ()
userManipTypeWidget :: UserManipType -> WidgetT App IO()
userManipTypeWidget t = do
	msgShow <- getMessageRender
	toWidget $ [hamlet|<i .#{getUserManipTypeIcon t} title=#{msgShow $ toAppMessage t} data-placement="bottom" rel="tooltip"> |]

