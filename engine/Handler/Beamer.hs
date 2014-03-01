{-# LANGUAGE OverloadedStrings #-}
module Handler.Beamer where

import Import
import qualified Prelude
import Text.Hamlet (hamletFile)
import Handler.Dominik
import PostGenerated

beamerLayout :: Text -> Widget -> Handler Html
beamerLayout title widget = do
	pc <- widgetToPageContent widget
	PageContent _ headTags bodyTags <- widgetToPageContent $ do
		toWidgetHead $(hamletFile "templates/beamer/head.hamlet")
		toWidget $(hamletFile "templates/beamer/symbols.hamlet")
		setTitle $ toHtml title
	giveUrlRenderer $(hamletFile "templates/beamer/overall.hamlet")

getBeamerSlidesR :: Handler Html
getBeamerSlidesR = do
	setUltDestCurrent	
	let slides =  [(minBound::BeamerSlidePublic)..(maxBound::BeamerSlidePublic)]
	privateSlides <- getPrivateSlides
	projectLayout $ toWidget $(whamletFile "templates/beamer/list.hamlet")
	where
		getPrivateSlides = do
			trusted <- isTrustedUser
			case trusted of
				Authorized -> return [(minBound::BeamerSlidePrivate)..(maxBound::BeamerSlidePrivate)]
				_ -> return []

getBeamerSlidePublicR :: BeamerSlidePublic -> Handler Html
getBeamerSlidePublicR = getBeamerSlideR
getBeamerSlidePrivateR :: BeamerSlidePrivate -> Handler Html
getBeamerSlidePrivateR slide = do
	trusted <- isTrustedUser
	case trusted of
		Authorized -> getBeamerSlideR slide
		_ -> permissionDenied "Login to obtain permission"

getBeamerSlideR :: (BeamerSlide a) => a -> Handler Html
getBeamerSlideR beamerslide = beamerLayout (getBeamerSlideTitle beamerslide) (toWidget $ getBeamerSlideWidget beamerslide)


