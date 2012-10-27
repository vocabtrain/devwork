module Generated (getQtGalleryImages, getJavaProjects, JavaProject, javaProjectId, javaProjectName, javaProjectArchive, javaProjectClass, javaProjectWidth, javaProjectHeight, javaProjectDescription, QtProject, qtProjectId, qtProjectName, qtProjectDescription, qtProjectArchive, getQtProjects)
	where

import Prelude

data JavaProject = JavaProject { 
	javaProjectId :: Int,
	javaProjectName :: String,
	javaProjectArchive :: String, 
	javaProjectClass :: String,
	javaProjectWidth :: Int,
	javaProjectHeight :: Int,
	javaProjectDescription :: String
	}
	deriving (Read, Show, Eq)

getQtGalleryImages :: [String]

getQtGalleryImages = [ "bb-clock","chemie_nmrdaten","diagramm","fourwin","fress-oder-stirb","frosch","gorilla2","gorilla","phonebook","pong","potential","roulette","schiffe-versenken","snake","spinglass","sudoku","tictactoe","trainer","vokabeltrainer","whack-him"]

getJavaProjects :: [JavaProject]
getJavaProjects = [ JavaProject{javaProjectId=0,javaProjectName="dodgeracer",javaProjectArchive="dodgeracer.jar",javaProjectClass="Racer.class",javaProjectWidth=350,javaProjectHeight=400,javaProjectDescription="dodgeracer.txt"},JavaProject{javaProjectId=1,javaProjectName="eifarbe",javaProjectArchive="eifarbe.jar",javaProjectClass="Eifarbe.class",javaProjectWidth=800,javaProjectHeight=400,javaProjectDescription="eifarbe.txt"},JavaProject{javaProjectId=2,javaProjectName="schiffeversenken",javaProjectArchive="schiffeversenken.jar",javaProjectClass="MyFrame.class",javaProjectWidth=800,javaProjectHeight=600,javaProjectDescription="schiffeversenken.txt"},JavaProject{javaProjectId=3,javaProjectName="wireking",javaProjectArchive="wireking.jar",javaProjectClass="T_MAIN.class",javaProjectWidth=1060,javaProjectHeight=830,javaProjectDescription="wireking.txt"}]

data QtProject = QtProject { 
	qtProjectId :: Int,
	qtProjectName :: String,
	qtProjectArchive :: String, 
	qtProjectDescription :: String
	}
	deriving (Read, Show, Eq)

getQtProjects :: [QtProject]
getQtProjects = [ QtProject{qtProjectId=0,qtProjectName="chemie_nmrdaten",qtProjectArchive="chemie_nmrdaten.tar.bz2",qtProjectDescription="Liest Peaks aus NMR-Daten"},QtProject{qtProjectId=1,qtProjectName="diagramm",qtProjectArchive="diagramm.tar.bz2",qtProjectDescription="Erstellt Diagramme"},QtProject{qtProjectId=2,qtProjectName="fourwin",qtProjectArchive="fourwin.tar.bz2",qtProjectDescription="Vier gewinnt im Spieler gegen Spieler Modus"},QtProject{qtProjectId=3,qtProjectName="frosch",qtProjectArchive="frosch.tar.bz2",qtProjectDescription="Ein Clone des Atari 2600 Spiels 'Die hungrigen Frösche'"},QtProject{qtProjectId=4,qtProjectName="particles",qtProjectArchive="particles.tar.bz2",qtProjectDescription="Gravitationsimulator von Partikelteilchen mit elektromagnetischer Wechselwirkung"},QtProject{qtProjectId=5,qtProjectName="trainer",qtProjectArchive="trainer.tar.bz2",qtProjectDescription="Trainiert Bundesländern, Vokabeln, etc."}]
