{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Dominik where

import Import
import qualified Prelude
import qualified Text.Blaze.Html
import GlobalLayout
import BibtexWidget

showTenth :: Int -> String
showTenth i
	| i < 10 = "0" ++ show i
	| otherwise = show i

getHomeR :: Handler Html
getHomeR = redirect DKHomeR

dominikLayoutSheet :: Widget -> SheetLayout App (Route App)
dominikLayoutSheet widget = SheetLayout 
	{ sheetTitle = "Dominik Köppl"
	, sheetNav = Nothing
	, sheetBanner =  Just $(whamletFile "templates/dominik/banner.hamlet")
	, sheetContent = widget
	}

courseLayout :: Widget -> Handler Html
courseLayout widget = globalLayout $ (dominikLayoutSheet widget) { sheetNav = Just $(whamletFile "templates/dominik/navcourse.hamlet") }
projectLayout :: Widget -> Handler Html
projectLayout widget = globalLayout $ (dominikLayoutSheet widget) { sheetNav = Just $(whamletFile "templates/dominik/navproject.hamlet") }


getQtProjectListR :: Handler Html
getQtProjectListR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtprojectlist.hamlet") 
	addStylesheet $ StaticR css_gallery_css

handleJavaProjectR :: Int -> Handler Html
handleJavaProjectR projectId
	| (projectId < 0 || projectId >= Prelude.length getJavaProjects) = invalidArgs [ "Wrong Project Id" ]
	| otherwise = courseLayout $ do
		setTitle $ Text.Blaze.Html.toHtml $ javaProjectName $ getJavaProjects !! projectId
		toWidget $(whamletFile "templates/dominik/javaproject.hamlet")
			where project = getJavaProjects !! projectId

getDKHomeR :: Handler Html
getDKHomeR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/homecourse.hamlet")
getDKProjectHomeR :: Handler Html

getOpenGLR :: Handler Html
getOpenGLR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/opengl.hamlet")

getQtGalleryR :: Handler Html
getQtGalleryR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtgallery.hamlet")
	addStylesheet $ StaticR css_gallery_css

getQtDescR :: Handler Html
getQtDescR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtdesc.hamlet") 

getQtOpenGLR :: Handler Html
getQtOpenGLR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtopengl.hamlet") 

getQtDossierR :: Handler Html
getQtDossierR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtkurs.hamlet") 

getJavaDescR :: Handler Html
getJavaDescR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/javadesc.hamlet") 

getJavaProjectListR :: Handler Html
getJavaProjectListR  = courseLayout $ do
	toWidget javaProjectList 

dominikContact :: Widget
dominikContact = toWidget $(whamletFile "templates/dominik/contact.hamlet") 
javaProjectList :: Widget
javaProjectList = toWidget $(whamletFile "templates/dominik/javaprojectlist.hamlet") 

getJavaDossierR :: Handler Html
getJavaDossierR = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/javakurs.hamlet") 

getTatoebaWebServiceR :: Handler Html
getTatoebaWebServiceR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/tatoebawebservice.hamlet") 
getTatoebaAppR :: Handler Html
getTatoebaAppR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/tatoebaapp.hamlet") 
getProjectAnnualR :: Handler Html
getProjectAnnualR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/annual.hamlet") 
getProjectFritzContactR :: Handler Html
getProjectFritzContactR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/fritzcontact.hamlet") 
getDKProjectHomeR  = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/homeproject.hamlet")
