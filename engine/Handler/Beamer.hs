{-# LANGUAGE OverloadedStrings #-}
module Handler.Beamer where

import Import
import qualified Prelude
import Text.Hamlet (hamletFile)
import Handler.Dominik
import PostGenerated

beamerLayout :: Text -> GWidget App App () -> GHandler App App RepHtml
beamerLayout title widget = do
	pc <- widgetToPageContent widget
	PageContent _ headTags bodyTags <- widgetToPageContent $ do
		toWidgetHead $(hamletFile "templates/beamer/head.hamlet")
		toWidget $(hamletFile "templates/beamer/symbols.hamlet")
		setTitle $ toHtml title
	hamletToRepHtml $(hamletFile "templates/beamer/overall.hamlet")

getBeamerSlidesR :: GHandler App App RepHtml
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

getBeamerSlidePublicR :: BeamerSlidePublic -> GHandler App App RepHtml
getBeamerSlidePublicR = getBeamerSlideR
getBeamerSlidePrivateR :: BeamerSlidePrivate -> GHandler App App RepHtml
getBeamerSlidePrivateR slide = do
	trusted <- isTrustedUser
	case trusted of
		Authorized -> getBeamerSlideR slide
		_ -> permissionDenied "Login to obtain permission"

getBeamerSlideR :: (BeamerSlide a) => a -> GHandler App App RepHtml
getBeamerSlideR beamerslide = beamerLayout (getBeamerSlideTitle beamerslide) (toWidget $ getBeamerSlideWidget beamerslide)
