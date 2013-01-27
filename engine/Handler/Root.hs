{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Root where

import Import

{-
import qualified Network.Wai as Wai
import Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as B64
basicHTTPAuthPrefix :: B.ByteString
basicHTTPAuthPrefix = "Basic "

data BasicAuthResult = BasicAuthAuthorized UserId | BasicAuthNothing | BasicAuthInvalidEncoding | BasicAuthWrongCreds

basicAuth :: GHandler App App BasicAuthResult
basicAuth = do 
	wr <- waiRequest
	case lookup "Authorization" $ Wai.requestHeaders wr of
		Nothing -> return BasicAuthNothing
		Just bstring -> do
			if B.isPrefixOf basicHTTPAuthPrefix bstring
				then do
					case B64.decode $ B.drop (B.length basicHTTPAuthPrefix) bstring of
						Left _ -> return BasicAuthInvalidEncoding
						Right string -> do
							case Text.splitOn ":" $ E.decodeUtf8 string of
								[user, password] -> do
									userResult <- runDB $ selectList [ UserNick ==. Just user, UserPassword ==. Just password] []
									if length userResult == 0
										then return BasicAuthWrongCreds
										else return $ BasicAuthAuthorized (entityKey $ userResult!!0)
								_ -> return BasicAuthInvalidEncoding
				else return BasicAuthInvalidEncoding



getFooR :: GHandler App App RepHtml
getFooR = do 
	wr <- waiRequest
	case lookup "Authorization" $ Wai.requestHeaders wr of
		Nothing -> do
			defaultLayout [whamlet| KEY empty |]
		Just bstring -> do
			if B.isPrefixOf basicHTTPAuthPrefix bstring
				then do
					case B64.decode $ B.drop (B.length basicHTTPAuthPrefix) bstring of
						Left string -> do
							defaultLayout [whamlet| KEY failed #{string} |]
						Right string -> do
							case Text.splitOn ":" $ E.decodeUtf8 string of
								[user, password] -> do
									userResult <- runDB $ selectList [ UserNick ==. Just user, UserPassword ==. Just password] []
									if length userResult == 0
										then do
											defaultLayout [whamlet| KEY failed NO #{user} -> #{password} |]
										else do
											defaultLayout [whamlet| KEY success #{user} -> #{password} |]
								_ -> do
									defaultLayout [whamlet| KEY wrong text |]
				else do
						defaultLayout [whamlet| KEY failed prefix |]
-}
{-
getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
        $(widgetFile "root/root")
-}
