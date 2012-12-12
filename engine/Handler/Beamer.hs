{-# LANGUAGE OverloadedStrings #-}
module Handler.Beamer where

import Import
import qualified Prelude
import Text.Hamlet (hamletFile)

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
	let slides =  [(minBound::BeamerSlide)..(maxBound::BeamerSlide)]
	defaultLayout $ toWidget $(whamletFile "templates/beamer/list.hamlet")

getBeamerSlideR :: BeamerSlide -> GHandler App App RepHtml
getBeamerSlideR beamerslide = beamerLayout (getBeamerSlideTitle beamerslide) (toWidget $ getBeamerSlideWidget beamerslide)

