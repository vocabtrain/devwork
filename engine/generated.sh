#!/bin/zsh

cat <<EOF
module Generated
	where

import Prelude
import Yesod (Route(..))
import Settings.StaticFiles
import qualified Yesod.Static
import Data.Text (Text)

data GalleryImage = GalleryImage 
	{ galleryImageSource :: Route Yesod.Static.Static
	, galleryImageThumbnail :: Route Yesod.Static.Static
	}

data JavaProject = JavaProject 
	{ javaProjectId :: Int
	, javaProjectName :: Text 
	, javaProjectArchive :: Route Yesod.Static.Static
	, javaProjectClass :: Text
	, javaProjectWidth :: Int
	, javaProjectHeight :: Int
	, javaProjectDescription :: Route Yesod.Static.Static
	}

data QtProject = QtProject 
	{ qtProjectId :: Int
	, qtProjectName :: Text 
	, qtProjectDescription :: Text
	, qtProjectPackage :: Route Yesod.Static.Static
	, qtProjectGalleryImage :: GalleryImage
	}

data WorkSheet = WorkSheet 
	{ workSheetNumber :: Int
	, workSheetSource :: Route Yesod.Static.Static
	, workSheetDocument :: Route Yesod.Static.Static
	}
data OpenGLQtLesson = OpenGLQtLesson 
	{ openGLQtLessonNumber :: Int
	, openGLQtLessonPackage :: Route Yesod.Static.Static
	, openGLQtLessonLink :: Text
	}
EOF

function getOpenGLQtLessons
{
	echo 'getOpenGLQtLessons :: [OpenGLQtLesson]'
	echo -n 'getOpenGLQtLessons = [ '
	a=()
	for i in `ls lesson*.zip`; do
		b=`basename $i .zip`
		unnum=`echo $b | grep -o '[0-9]\+$'`
		num=`echo $unnum | bc`
		escaped=`trim "$b"`
		a+="OpenGLQtLesson{openGLQtLessonNumber=$num,openGLQtLessonPackage=bin_pkg_nehe_${escaped}_zip,openGLQtLessonLink=\"http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=${unnum}\"}"
	done
	echo -n $a | sed 's@ @,@g'
	echo "]"
}

function trim
{
	echo $1 | tr '.\-/' '_'
}

function getQtGalleryImages
{
	echo "getQtGalleryImages :: [GalleryImage]"
	echo -n "getQtGalleryImages = [ "
	a=()
	for i in `ls *.jpg`; do
		[[ -n `echo $i | grep "_thumb.jpg"` ]] && continue
		b=`basename $i .jpg`
		[[ -n `ls ${b}_thumb.jpg` ]] || continue
		escaped=`trim "$b"`
		a+="GalleryImage{galleryImageSource=img_qtgallery_${escaped}_jpg,galleryImageThumbnail=img_qtgallery_${escaped}_thumb_jpg}"
	done
	echo -n $a | sed 's@ @,@g'
	echo "]"
}

function getWorksheets
{
	echo "$1 :: [WorkSheet]"
	echo -n "$1 = [ "
	a=()
	for i in `ls "$2"*.tex`; do
		b=`basename $i .tex`
		[[ -n `ls $b.pdf` ]] || continue
		num=`echo $b | grep -o '[0-9]\+$' | bc`
		escaped=`trim "$b"`
		a+="WorkSheet{workSheetNumber=$num,workSheetSource=bin_script_${escaped}_tex,workSheetDocument=bin_script_${escaped}_pdf}"
	done
	echo -n $a | sed 's@ @,@g'
	echo "]"
}

function getQtProjects
{
	echo 'getQtProjects :: [QtProject]'
	echo -n "getQtProjects = [ "
	id=0
	a=()
	for i in `ls *.txt`; do
		base=`basename $i .txt`
		escaped=`trim "$base"`
		desc=`cat $i | sed 's@ @%@g'`
		a+="QtProject{qtProjectId=$id,qtProjectName=\"$base\",qtProjectPackage=bin_projects_qt_${escaped}_tar_bz2,qtProjectDescription=\"$desc\",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_${escaped}_jpg,galleryImageThumbnail=img_qtgallery_${escaped}_thumb_jpg}}"
		((++id))
	done
	echo -n $a | sed 's@ @,@g' | tr '%' ' '
	echo "]"
}

function getJavaProjects
{
	echo 'getJavaProjects :: [JavaProject]'
	echo -n "getJavaProjects = [ "
	id=0
	a=()
	for i in `ls *.ini`; do
		cont=`cat $i | tr '\n' ';'`
		class=`echo $cont | cut -d';' -f1`
		width=`echo $cont | cut -d';' -f2`
		height=`echo $cont | cut -d';' -f3`
		base=`basename $i .ini`
		escaped=`trim "$base"`
		
	a+="JavaProject{javaProjectId=$id,javaProjectName=\"$base\",javaProjectArchive=bin_projects_java_${escaped}_jar,javaProjectClass=\"$class\",javaProjectWidth=$width,javaProjectHeight=$height,javaProjectDescription=bin_projects_java_${escaped}_txt}"
		((++id))
	done
	echo -n $a | sed 's@ @,@g'
	echo "]"
}

p=`pwd`
cd "$p/static/img/qtgallery" && getQtGalleryImages
cd "$p/static/bin/projects/java" && getJavaProjects
cd "$p/static/bin/projects/qt" && getQtProjects
cd "$p/static/bin/script" && getWorksheets getJavaWorkSheets 'java_blatt'
cd "$p/static/bin/script" && getWorksheets getQtWorkSheets 'qt_blatt'
cd "$p/static/bin/pkg/nehe" && getOpenGLQtLessons
