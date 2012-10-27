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
	setTitle "Qt Projekte"
	toWidget $(whamletFile "templates/dominik/qtprojectlist.hamlet") 
	addStylesheetRemote $ (staticServer "styles/gallery.css")

handleJavaProjectR :: Int -> GHandler App App RepHtml
handleJavaProjectR projectId
	| (projectId < 0 || projectId >= Prelude.length getJavaProjects) = invalidArgs [ "Wrong Project Id" ]
	| otherwise = myLayout $ do
		setTitle $ Text.Blaze.Html.toHtml $ javaProjectName $ getJavaProjects !! projectId
		toWidget $(whamletFile "templates/dominik/javaproject.hamlet")
			where project = getJavaProjects !! projectId

getDKHomeR :: GHandler App App RepHtml
getDKHomeR  = myLayout $ do
	setTitle "Kurshomepage"
	toWidget $(whamletFile "templates/dominik/home.hamlet")

getOpenGLR :: GHandler App App RepHtml
getOpenGLR  = myLayout $ do
	setTitle "OpenGL"
	toWidget $(whamletFile "templates/dominik/opengl.hamlet")

getQtGalleryR :: GHandler App App RepHtml
getQtGalleryR  = myLayout $ do
	setTitle "Qt Galerie"
	toWidget $(whamletFile "templates/dominik/qtgallery.hamlet")
	addStylesheetRemote $ staticServer "styles/gallery.css"
		where pictures = getQtGalleryImages

getQtDescR :: GHandler App App RepHtml
getQtDescR  = myLayout $ do
	setTitle "Qt Kurs"
	toWidget $(whamletFile "templates/dominik/qtdesc.hamlet") 

getQtOpenGLR :: GHandler App App RepHtml
getQtOpenGLR  = myLayout $ do
	setTitle "Qt OpenGL"
	toWidget $(whamletFile "templates/dominik/qtopengl.hamlet") 
		where worksheets = [1..12] :: [Int]

getQtDossierR :: GHandler App App RepHtml
getQtDossierR  = myLayout $ do
	setTitle "Qt Unterlagen"
	toWidget $(whamletFile "templates/dominik/qtkurs.hamlet") 
		where worksheets = [1..8] :: [Int]

getJavaDescR :: GHandler App App RepHtml
getJavaDescR  = myLayout $ do
	setTitle "Java Kurs"
	toWidget $(whamletFile "templates/dominik/javadesc.hamlet") 

getJavaProjectListR :: GHandler App App RepHtml
getJavaProjectListR  = myLayout $ do
	setTitle "Java Projekte"
	toWidget javaProjectList 

javaProjectList :: GWidget App App ()
javaProjectList = do
	toWidget $(whamletFile "templates/dominik/javaprojectlist.hamlet") 

getJavaDossierR :: GHandler App App RepHtml
getJavaDossierR = myLayout $ do
	setTitle "Java Unterlagen"
	toWidget $(whamletFile "templates/dominik/javakurs.hamlet") 
		where worksheets = [1..13] :: [Int]
