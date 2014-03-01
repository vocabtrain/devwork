module GlobalLayout
	( SheetLayout (..)
	, globalLayout'
	, globalLayout
	) where

import Import
import Text.Hamlet (hamletFile)
import Yesod.Auth
import qualified Data.Text as Text
import Breadcrumbs ()

data SheetLayout sub url = SheetLayout {
	  sheetTitle :: Text
	, sheetNav :: Maybe Widget
	, sheetBanner :: Maybe Widget
	, sheetContent :: Widget
}

globalLayout' :: Text -> Widget -> Handler Html
globalLayout' title widget = globalLayout $ SheetLayout title Nothing Nothing widget




globalLayout :: SheetLayout sub (Route App) -> Handler Html
--globalLayout subtitle contents = do
globalLayout sheet = do
	master <- getYesod
	mmsg <- getMessage
	content <- widgetToPageContent $ sheetContent sheet
	mnav <- readSheetWidget $ sheetNav sheet
	mbanner <- readSheetWidget $ sheetBanner sheet
	maid <- maybeAuthId
	(page_title, page_parents) <- breadcrumbs

	PageContent title headTags bodyTags <- widgetToPageContent $ do
		toWidgetHead $(hamletFile "templates/skeleton/head.hamlet")
		setTitle . toHtml $ sheetTitle sheet
		addStylesheet $ StaticR css_bootstrap_css
		addStylesheet $ StaticR css_bootstrap_docs_css
		addStylesheet $ StaticR css_normalize_css
		addStylesheet $ StaticR css_flags_css
		addStylesheet $ StaticR css_main_css
	giveUrlRenderer $(hamletFile "templates/skeleton/overall.hamlet")
	where
		readSheetWidget :: Maybe Widget -> Handler (Maybe (PageContent (Route App)))
		readSheetWidget mwidget =
			case mwidget of
				Nothing -> return Nothing
				Just w -> do 
					v <- widgetToPageContent w
					return $ Just v

