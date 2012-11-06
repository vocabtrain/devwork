{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Dominik where

import Import
import qualified Prelude
import Text.Hamlet (hamletFile)
import qualified Text.Blaze.Html


showTenth :: Int -> String
showTenth i
	| i < 10 = "0" ++ show i
	| otherwise = show i


myLayout :: GWidget App App () -> GHandler App App RepHtml
myLayout widget = globalLayout $ SheetLayout { 
	  sheetTitle = "Dominik Köppl"
	, sheetNav = Just $(hamletFile "templates/dominik/nav.hamlet")
	, sheetBanner =  Just $(hamletFile "templates/dominik/banner.hamlet")
	, sheetContent = widget
	}


getQtProjectListR :: GHandler App App RepHtml
getQtProjectListR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/qtprojectlist.hamlet") 
	addStylesheet $ StaticR css_gallery_css

handleJavaProjectR :: Int -> GHandler App App RepHtml
handleJavaProjectR projectId
	| (projectId < 0 || projectId >= Prelude.length getJavaProjects) = invalidArgs [ "Wrong Project Id" ]
	| otherwise = myLayout $ do
		setTitle $ Text.Blaze.Html.toHtml $ javaProjectName $ getJavaProjects !! projectId
		toWidget $(whamletFile "templates/dominik/javaproject.hamlet")
			where project = getJavaProjects !! projectId

getDKHomeR :: GHandler App App RepHtml
getDKHomeR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/home.hamlet")

getOpenGLR :: GHandler App App RepHtml
getOpenGLR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/opengl.hamlet")

getQtGalleryR :: GHandler App App RepHtml
getQtGalleryR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/qtgallery.hamlet")
	addStylesheet $ StaticR css_gallery_css

getQtDescR :: GHandler App App RepHtml
getQtDescR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/qtdesc.hamlet") 

getQtOpenGLR :: GHandler App App RepHtml
getQtOpenGLR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/qtopengl.hamlet") 

getQtDossierR :: GHandler App App RepHtml
getQtDossierR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/qtkurs.hamlet") 

getJavaDescR :: GHandler App App RepHtml
getJavaDescR  = myLayout $ do
	toWidget $(whamletFile "templates/dominik/javadesc.hamlet") 

getJavaProjectListR :: GHandler App App RepHtml
getJavaProjectListR  = myLayout $ do
	toWidget javaProjectList 

javaProjectList :: GWidget App App ()
javaProjectList = do
	toWidget $(whamletFile "templates/dominik/javaprojectlist.hamlet") 

getJavaDossierR :: GHandler App App RepHtml
getJavaDossierR = myLayout $ do
	toWidget $(whamletFile "templates/dominik/javakurs.hamlet") 
