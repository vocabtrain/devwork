#!/bin/zsh

p=`pwd`
datadir=/home/niki/data/
scriptdir=/home/niki/devwork/cron
beamerpublicslidesdir=templates/beamer/public
beamerprivateslidesdir=templates/beamer/private

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
function getTatoebaLanguagesPost
{
##toAppMessage
	echo "instance ToAppMessage TatoebaLanguage where "
	for i in `cat "$datadir/available_languages.txt" | sed 's@ @_@g'`; do
		l=`echo $i | cut -d'@' -f1`
		L=`echo $l | tr '[:lower:]' '[:upper:]'`
		echo "\ttoAppMessage LANG_$L = MsgLang_$l"
	done
}

function getTatoebaLanguages
{
	echo -n 'data TatoebaLanguage = '
	a=()
	for i in `cat "$datadir/available_languages.txt" | sed 's@ @_@g'`; do
		a+="LANG_`echo $i | cut -d'@' -f1 | tr '[:lower:]' '[:upper:]'`"
	done
	echo $a | sed 's@ @\n\t | @g'
	echo "\tderiving(Eq,Enum,Bounded)"
##instance Read
	echo "instance Read TatoebaLanguage where "
	echo "\treadPrec = choice $ strValMap ["
	a=()
	for i in `cat "$datadir/available_languages.txt" | sed 's@ @_@g'`; do
		a+="(\"`echo $i | cut -d'@' -f1`\",LANG_`echo $i | cut -d'@' -f1 | tr '[:lower:]' '[:upper:]'`)"
	done
	echo  "\t\t$a" | sed 's@ @\n\t\t, @g'
cat <<EOF
		]
		where
			strValMap :: [(String, TatoebaLanguage)] -> [ReadPrec TatoebaLanguage]
			strValMap = map (\(x, y) -> lift $ string x >> return y)
EOF
##
##instance Show
	echo 'instance Show TatoebaLanguage where'
	a=()
	for i in `cat "$datadir/available_languages.txt" | sed 's@ @_@g'`; do
		a+="show(LANG_`echo $i | cut -d'@' -f1 | tr '[:lower:]' '[:upper:]'`)=\"`echo $i | cut -d'@' -f1`\""
	done
	echo "\t$a" | sed 's@ @\n\t@g'
#	echo "\t\t|otherwise = \"und\""
	
	echo 'derivePersistField "TatoebaLanguage"'

	##
#	##getTatoebaLanguageName
#	echo 'getTatoebaLanguageName :: TatoebaLanguage -> Text'
#	a=()
#	for i in `cat "$datadir/available_languages.txt" | sed 's@ @_@g'`; do
#		a+="getTatoebaLanguageName(LANG_`echo $i | cut -d'@' -f1 | tr '[:lower:]' '[:upper:]'`)=\"`echo $i | cut -d'@' -f2`\""
#	done
#	echo "$a" | sed 's@ @\n@g'
#	echo "\t|otherwise = \"unknown language\""
}

function beamerSlides
{
	echo -n "data $1 = "
	a=()
	files=()
	for i in `ls *.hamlet`; do
		files+="$i"
		e=`echo "$i:r" | tr '[a-z]' '[A-Z]'`
		a+="BEAMER_SILDE_$e"
	done
	echo $a | sed 's@ @|@g'
	echo "\tderiving(Show,Eq,Read,Enum,Bounded)"
}
function beamerSlidesPost
{
	a=()
	files=()
	for i in `ls *.hamlet`; do
		files+="$i"
		e=`echo "$i:r" | tr '[a-z]' '[A-Z]'`
		a+="BEAMER_SILDE_$e"
	done
	echo "instance BeamerSlide $1 where"
	max="$a[(I)$a[-1]]"
	for i in `seq 1 $max`; do
		e=`echo "$i:r" | tr '[a-z]' '[A-Z]'`
		echo "\tgetBeamerSlideWidget $a[$i] = \$(whamletFile \"$2/$files[$i]\")"
	done
}

function createPathPieceInstance
{
	echo "instance PathPiece $1 where"
cat <<EOF
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show
EOF
}


function generateHs {
cat <<EOF
module Generated
	where

import Prelude
import Yesod (Route(..))
import Settings.StaticFiles
import qualified Yesod.Static
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.TH
import Database.Persist
import Text.Read
import Text.ParserCombinators.ReadP hiding (choice)
import Web.PathPieces
import ShowText ()

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
	getTatoebaLanguages

	cd "$p/static/img/qtgallery" && getQtGalleryImages
	cd "$p/static/bin/projects/java" && getJavaProjects
	cd "$p/static/bin/projects/qt" && getQtProjects
	cd "$p/static/bin/script" && getWorksheets getJavaWorkSheets 'java_blatt'
	cd "$p/static/bin/script" && getWorksheets getQtWorkSheets 'qt_blatt'
	cd "$p/static/bin/pkg/nehe" && getOpenGLQtLessons
	cd "$p/$beamerpublicslidesdir" && beamerSlides "BeamerSlidePublic" "$beamerpublicslidesdir"
	cd "$p/$beamerprivateslidesdir" && beamerSlides "BeamerSlidePrivate" "$beamerprivateslidesdir"
	createPathPieceInstance TatoebaLanguage
	createPathPieceInstance BeamerSlidePrivate
	createPathPieceInstance BeamerSlidePublic
} #GenerateHS

function generatePostHs {
cat  <<EOF
module PostGenerated
	where

import Prelude
import Yesod 
import ToAppMessage
import Foundation
import Generated
import Settings.StaticFiles
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List as List

class (Show a, Read a) => BeamerSlide a where
	getBeamerSlideTitle :: a -> Text
	getBeamerSlideTitle slide = Text.intercalate " " $ List.drop 2 $ map (\word -> Text.cons (Char.toUpper . Text.head $ word) (Text.tail word)) $ Text.words $ Text.toLower $ Text.replace "_" " " $ Text.pack $ show slide
	getBeamerSlideWidget :: a -> GWidget App App ()

EOF
	cd "$p/$beamerpublicslidesdir" && beamerSlidesPost "BeamerSlidePublic" "$beamerpublicslidesdir"
	cd "$p/$beamerprivateslidesdir" && beamerSlidesPost "BeamerSlidePrivate" "$beamerprivateslidesdir"
	getTatoebaLanguagesPost
} # generatePostHs

generateHs | tee "$p/Generated.hs"
generatePostHs | tee "$p/PostGenerated.hs"

#instance ToMarkup TatoebaLanguage where
#	toMarkup = toMarkup . getTatoebaLanguageName

