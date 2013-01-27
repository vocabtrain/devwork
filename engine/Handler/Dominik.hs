{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Dominik where

import Import
import qualified Prelude
import qualified Text.Blaze.Html

showTenth :: Int -> String
showTenth i
	| i < 10 = "0" ++ show i
	| otherwise = show i

getHomeR :: GHandler App App RepHtml
getHomeR = redirect DKHomeR

dominikLayoutSheet :: GWidget App App () -> SheetLayout App (Route App)
dominikLayoutSheet widget = SheetLayout 
	{ sheetTitle = "Dominik Köppl"
	, sheetNav = Nothing
	, sheetBanner =  Just $(whamletFile "templates/dominik/banner.hamlet")
	, sheetContent = widget
	}

courseLayout :: GWidget App App () -> GHandler App App RepHtml
courseLayout widget = globalLayout $ (dominikLayoutSheet widget) { sheetNav = Just $(whamletFile "templates/dominik/navcourse.hamlet") }
projectLayout :: GWidget App App () -> GHandler App App RepHtml
projectLayout widget = globalLayout $ (dominikLayoutSheet widget) { sheetNav = Just $(whamletFile "templates/dominik/navproject.hamlet") }


getQtProjectListR :: GHandler App App RepHtml
getQtProjectListR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtprojectlist.hamlet") 
	addStylesheet $ StaticR css_gallery_css

handleJavaProjectR :: Int -> GHandler App App RepHtml
handleJavaProjectR projectId
	| (projectId < 0 || projectId >= Prelude.length getJavaProjects) = invalidArgs [ "Wrong Project Id" ]
	| otherwise = courseLayout $ do
		setTitle $ Text.Blaze.Html.toHtml $ javaProjectName $ getJavaProjects !! projectId
		toWidget $(whamletFile "templates/dominik/javaproject.hamlet")
			where project = getJavaProjects !! projectId

getDKHomeR :: GHandler App App RepHtml
getDKHomeR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/homecourse.hamlet")
getDKProjectHomeR :: GHandler App App RepHtml

getOpenGLR :: GHandler App App RepHtml
getOpenGLR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/opengl.hamlet")

getQtGalleryR :: GHandler App App RepHtml
getQtGalleryR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtgallery.hamlet")
	addStylesheet $ StaticR css_gallery_css

getQtDescR :: GHandler App App RepHtml
getQtDescR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtdesc.hamlet") 

getQtOpenGLR :: GHandler App App RepHtml
getQtOpenGLR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtopengl.hamlet") 

getQtDossierR :: GHandler App App RepHtml
getQtDossierR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/qtkurs.hamlet") 

getJavaDescR :: GHandler App App RepHtml
getJavaDescR  = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/javadesc.hamlet") 

getJavaProjectListR :: GHandler App App RepHtml
getJavaProjectListR  = courseLayout $ do
	toWidget javaProjectList 

dominikContact :: GWidget App App ()
dominikContact = toWidget $(whamletFile "templates/dominik/contact.hamlet") 
javaProjectList :: GWidget App App ()
javaProjectList = toWidget $(whamletFile "templates/dominik/javaprojectlist.hamlet") 

getJavaDossierR :: GHandler App App RepHtml
getJavaDossierR = courseLayout $ do
	toWidget $(whamletFile "templates/dominik/javakurs.hamlet") 

getTatoebaWebServiceR :: GHandler App App RepHtml
getTatoebaWebServiceR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/tatoebawebservice.hamlet") 
getTatoebaAppR :: GHandler App App RepHtml
getTatoebaAppR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/tatoebaapp.hamlet") 
getProjectAnnualR :: GHandler App App RepHtml
getProjectAnnualR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/annual.hamlet") 
getProjectFritzContactR :: GHandler App App RepHtml
getProjectFritzContactR = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/fritzcontact.hamlet") 
getDKProjectHomeR  = projectLayout $ do
	toWidget $(whamletFile "templates/dominik/homeproject.hamlet")
