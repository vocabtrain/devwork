module Generated
	where

import Prelude
import Yesod (Route(..))
import Settings.StaticFiles
import qualified Yesod.Static
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.List as List
import Text.Hamlet (hamletFile)
import Database.Persist.TH
import Database.Persist
import Text.Read
import Text.ParserCombinators.ReadP hiding (choice)
import Text.Blaze
import Web.PathPieces

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
data TatoebaLanguage = LANG_ARA
	 | LANG_ARZ
	 | LANG_BEL
	 | LANG_CES
	 | LANG_CMN
	 | LANG_DAN
	 | LANG_DEU
	 | LANG_ELL
	 | LANG_ENG
	 | LANG_EPO
	 | LANG_EST
	 | LANG_EUS
	 | LANG_FIN
	 | LANG_FRA
	 | LANG_HEB
	 | LANG_HIN
	 | LANG_HSB
	 | LANG_HUN
	 | LANG_IDO
	 | LANG_INA
	 | LANG_ISL
	 | LANG_ITA
	 | LANG_JBO
	 | LANG_JPN
	 | LANG_KOR
	 | LANG_LAT
	 | LANG_LIT
	 | LANG_LZH
	 | LANG_MAL
	 | LANG_NDS
	 | LANG_NLD
	 | LANG_NOB
	 | LANG_PES
	 | LANG_POL
	 | LANG_POR
	 | LANG_RON
	 | LANG_RUS
	 | LANG_SPA
	 | LANG_SRP
	 | LANG_SWE
	 | LANG_SWH
	 | LANG_TLH
	 | LANG_TUR
	 | LANG_UIG
	 | LANG_UKR
	 | LANG_UZB
	 | LANG_VIE
	 | LANG_WUU
	 | LANG_XAL
	 | LANG_YUE
	 | LANG_ZSM
	 | LANG_UND
	deriving(Eq,Enum,Bounded)
instance Read TatoebaLanguage where 
	readPrec = choice $ strValMap [
		("ara",LANG_ARA)
		, ("arz",LANG_ARZ)
		, ("bel",LANG_BEL)
		, ("ces",LANG_CES)
		, ("cmn",LANG_CMN)
		, ("dan",LANG_DAN)
		, ("deu",LANG_DEU)
		, ("ell",LANG_ELL)
		, ("eng",LANG_ENG)
		, ("epo",LANG_EPO)
		, ("est",LANG_EST)
		, ("eus",LANG_EUS)
		, ("fin",LANG_FIN)
		, ("fra",LANG_FRA)
		, ("heb",LANG_HEB)
		, ("hin",LANG_HIN)
		, ("hsb",LANG_HSB)
		, ("hun",LANG_HUN)
		, ("ido",LANG_IDO)
		, ("ina",LANG_INA)
		, ("isl",LANG_ISL)
		, ("ita",LANG_ITA)
		, ("jbo",LANG_JBO)
		, ("jpn",LANG_JPN)
		, ("kor",LANG_KOR)
		, ("lat",LANG_LAT)
		, ("lit",LANG_LIT)
		, ("lzh",LANG_LZH)
		, ("mal",LANG_MAL)
		, ("nds",LANG_NDS)
		, ("nld",LANG_NLD)
		, ("nob",LANG_NOB)
		, ("pes",LANG_PES)
		, ("pol",LANG_POL)
		, ("por",LANG_POR)
		, ("ron",LANG_RON)
		, ("rus",LANG_RUS)
		, ("spa",LANG_SPA)
		, ("srp",LANG_SRP)
		, ("swe",LANG_SWE)
		, ("swh",LANG_SWH)
		, ("tlh",LANG_TLH)
		, ("tur",LANG_TUR)
		, ("uig",LANG_UIG)
		, ("ukr",LANG_UKR)
		, ("uzb",LANG_UZB)
		, ("vie",LANG_VIE)
		, ("wuu",LANG_WUU)
		, ("xal",LANG_XAL)
		, ("yue",LANG_YUE)
		, ("zsm",LANG_ZSM)
		, ("und",LANG_UND)
		]
		where
			strValMap :: [(String, TatoebaLanguage)] -> [ReadPrec TatoebaLanguage]
			strValMap = map (\(x, y) -> lift $ string x >> return y)
instance Show TatoebaLanguage where
	show(LANG_ARA)="ara"
	show(LANG_ARZ)="arz"
	show(LANG_BEL)="bel"
	show(LANG_CES)="ces"
	show(LANG_CMN)="cmn"
	show(LANG_DAN)="dan"
	show(LANG_DEU)="deu"
	show(LANG_ELL)="ell"
	show(LANG_ENG)="eng"
	show(LANG_EPO)="epo"
	show(LANG_EST)="est"
	show(LANG_EUS)="eus"
	show(LANG_FIN)="fin"
	show(LANG_FRA)="fra"
	show(LANG_HEB)="heb"
	show(LANG_HIN)="hin"
	show(LANG_HSB)="hsb"
	show(LANG_HUN)="hun"
	show(LANG_IDO)="ido"
	show(LANG_INA)="ina"
	show(LANG_ISL)="isl"
	show(LANG_ITA)="ita"
	show(LANG_JBO)="jbo"
	show(LANG_JPN)="jpn"
	show(LANG_KOR)="kor"
	show(LANG_LAT)="lat"
	show(LANG_LIT)="lit"
	show(LANG_LZH)="lzh"
	show(LANG_MAL)="mal"
	show(LANG_NDS)="nds"
	show(LANG_NLD)="nld"
	show(LANG_NOB)="nob"
	show(LANG_PES)="pes"
	show(LANG_POL)="pol"
	show(LANG_POR)="por"
	show(LANG_RON)="ron"
	show(LANG_RUS)="rus"
	show(LANG_SPA)="spa"
	show(LANG_SRP)="srp"
	show(LANG_SWE)="swe"
	show(LANG_SWH)="swh"
	show(LANG_TLH)="tlh"
	show(LANG_TUR)="tur"
	show(LANG_UIG)="uig"
	show(LANG_UKR)="ukr"
	show(LANG_UZB)="uzb"
	show(LANG_VIE)="vie"
	show(LANG_WUU)="wuu"
	show(LANG_XAL)="xal"
	show(LANG_YUE)="yue"
	show(LANG_ZSM)="zsm"
	show(LANG_UND)="und"
derivePersistField "TatoebaLanguage"
getTatoebaLanguageName :: TatoebaLanguage -> Text
getTatoebaLanguageName(LANG_ARA)="Arabic"
getTatoebaLanguageName(LANG_ARZ)="arz"
getTatoebaLanguageName(LANG_BEL)="Belarusian"
getTatoebaLanguageName(LANG_CES)="Czech"
getTatoebaLanguageName(LANG_CMN)="cmn"
getTatoebaLanguageName(LANG_DAN)="Danish"
getTatoebaLanguageName(LANG_DEU)="German"
getTatoebaLanguageName(LANG_ELL)="Greek,_Modern_(1453-)"
getTatoebaLanguageName(LANG_ENG)="English"
getTatoebaLanguageName(LANG_EPO)="Esperanto"
getTatoebaLanguageName(LANG_EST)="Estonian"
getTatoebaLanguageName(LANG_EUS)="Basque"
getTatoebaLanguageName(LANG_FIN)="Finnish"
getTatoebaLanguageName(LANG_FRA)="French"
getTatoebaLanguageName(LANG_HEB)="Hebrew"
getTatoebaLanguageName(LANG_HIN)="Hindi"
getTatoebaLanguageName(LANG_HSB)="Upper_Sorbian"
getTatoebaLanguageName(LANG_HUN)="Hungarian"
getTatoebaLanguageName(LANG_IDO)="Ido"
getTatoebaLanguageName(LANG_INA)="Interlingua_(International_Auxiliary_Language_Association)"
getTatoebaLanguageName(LANG_ISL)="Icelandic"
getTatoebaLanguageName(LANG_ITA)="Italian"
getTatoebaLanguageName(LANG_JBO)="Lojban"
getTatoebaLanguageName(LANG_JPN)="Japanese"
getTatoebaLanguageName(LANG_KOR)="Korean"
getTatoebaLanguageName(LANG_LAT)="Latin"
getTatoebaLanguageName(LANG_LIT)="Lithuanian"
getTatoebaLanguageName(LANG_LZH)="lzh"
getTatoebaLanguageName(LANG_MAL)="Malayalam"
getTatoebaLanguageName(LANG_NDS)="Low_German;_Low_Saxon;_German,_Low;_Saxon,_Low"
getTatoebaLanguageName(LANG_NLD)="Dutch;_Flemish"
getTatoebaLanguageName(LANG_NOB)="Bokmål,_Norwegian;_Norwegian_Bokmål"
getTatoebaLanguageName(LANG_PES)="pes"
getTatoebaLanguageName(LANG_POL)="Polish"
getTatoebaLanguageName(LANG_POR)="Portuguese"
getTatoebaLanguageName(LANG_RON)="Romanian;_Moldavian;_Moldovan"
getTatoebaLanguageName(LANG_RUS)="Russian"
getTatoebaLanguageName(LANG_SPA)="Spanish;_Castilian"
getTatoebaLanguageName(LANG_SRP)="Serbian"
getTatoebaLanguageName(LANG_SWE)="Swedish"
getTatoebaLanguageName(LANG_SWH)="swh"
getTatoebaLanguageName(LANG_TLH)="Klingon;_tlhIngan-Hol"
getTatoebaLanguageName(LANG_TUR)="Turkish"
getTatoebaLanguageName(LANG_UIG)="Uighur;_Uyghur"
getTatoebaLanguageName(LANG_UKR)="Ukrainian"
getTatoebaLanguageName(LANG_UZB)="Uzbek"
getTatoebaLanguageName(LANG_VIE)="Vietnamese"
getTatoebaLanguageName(LANG_WUU)="wuu"
getTatoebaLanguageName(LANG_XAL)="Kalmyk;_Oirat"
getTatoebaLanguageName(LANG_YUE)="yue"
getTatoebaLanguageName(LANG_ZSM)="zsm"
getTatoebaLanguageName(LANG_UND)="Undefined"
getQtGalleryImages :: [GalleryImage]
getQtGalleryImages = [ GalleryImage{galleryImageSource=img_qtgallery_bb_clock_jpg,galleryImageThumbnail=img_qtgallery_bb_clock_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_chemie_nmrdaten_jpg,galleryImageThumbnail=img_qtgallery_chemie_nmrdaten_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_diagramm_jpg,galleryImageThumbnail=img_qtgallery_diagramm_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_fourwin_jpg,galleryImageThumbnail=img_qtgallery_fourwin_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_fress_oder_stirb_jpg,galleryImageThumbnail=img_qtgallery_fress_oder_stirb_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_frosch_jpg,galleryImageThumbnail=img_qtgallery_frosch_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_gorilla2_jpg,galleryImageThumbnail=img_qtgallery_gorilla2_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_gorilla_jpg,galleryImageThumbnail=img_qtgallery_gorilla_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_particles_jpg,galleryImageThumbnail=img_qtgallery_particles_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_phonebook_jpg,galleryImageThumbnail=img_qtgallery_phonebook_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_pong_jpg,galleryImageThumbnail=img_qtgallery_pong_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_potential_jpg,galleryImageThumbnail=img_qtgallery_potential_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_roulette_jpg,galleryImageThumbnail=img_qtgallery_roulette_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_schiffe_versenken_jpg,galleryImageThumbnail=img_qtgallery_schiffe_versenken_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_snake_jpg,galleryImageThumbnail=img_qtgallery_snake_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_spinglass_jpg,galleryImageThumbnail=img_qtgallery_spinglass_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_sudoku_jpg,galleryImageThumbnail=img_qtgallery_sudoku_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_tictactoe_jpg,galleryImageThumbnail=img_qtgallery_tictactoe_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_trainer_jpg,galleryImageThumbnail=img_qtgallery_trainer_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_vokabeltrainer_jpg,galleryImageThumbnail=img_qtgallery_vokabeltrainer_thumb_jpg},GalleryImage{galleryImageSource=img_qtgallery_whack_him_jpg,galleryImageThumbnail=img_qtgallery_whack_him_thumb_jpg}]
getJavaProjects :: [JavaProject]
getJavaProjects = [ JavaProject{javaProjectId=0,javaProjectName="dodgeracer",javaProjectArchive=bin_projects_java_dodgeracer_jar,javaProjectClass="Racer.class",javaProjectWidth=350,javaProjectHeight=400,javaProjectDescription=bin_projects_java_dodgeracer_txt},JavaProject{javaProjectId=1,javaProjectName="eifarbe",javaProjectArchive=bin_projects_java_eifarbe_jar,javaProjectClass="Eifarbe.class",javaProjectWidth=800,javaProjectHeight=400,javaProjectDescription=bin_projects_java_eifarbe_txt},JavaProject{javaProjectId=2,javaProjectName="schiffeversenken",javaProjectArchive=bin_projects_java_schiffeversenken_jar,javaProjectClass="MyFrame.class",javaProjectWidth=800,javaProjectHeight=600,javaProjectDescription=bin_projects_java_schiffeversenken_txt},JavaProject{javaProjectId=3,javaProjectName="wireking",javaProjectArchive=bin_projects_java_wireking_jar,javaProjectClass="T_MAIN.class",javaProjectWidth=1060,javaProjectHeight=830,javaProjectDescription=bin_projects_java_wireking_txt}]
getQtProjects :: [QtProject]
getQtProjects = [ QtProject{qtProjectId=0,qtProjectName="chemie_nmrdaten",qtProjectPackage=bin_projects_qt_chemie_nmrdaten_tar_bz2,qtProjectDescription="Liest Peaks aus NMR-Daten",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_chemie_nmrdaten_jpg,galleryImageThumbnail=img_qtgallery_chemie_nmrdaten_thumb_jpg}},QtProject{qtProjectId=1,qtProjectName="diagramm",qtProjectPackage=bin_projects_qt_diagramm_tar_bz2,qtProjectDescription="Erstellt Diagramme",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_diagramm_jpg,galleryImageThumbnail=img_qtgallery_diagramm_thumb_jpg}},QtProject{qtProjectId=2,qtProjectName="fourwin",qtProjectPackage=bin_projects_qt_fourwin_tar_bz2,qtProjectDescription="Vier gewinnt im Spieler gegen Spieler Modus",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_fourwin_jpg,galleryImageThumbnail=img_qtgallery_fourwin_thumb_jpg}},QtProject{qtProjectId=3,qtProjectName="frosch",qtProjectPackage=bin_projects_qt_frosch_tar_bz2,qtProjectDescription="Ein Clone des Atari 2600 Spiels 'Die hungrigen Frösche'",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_frosch_jpg,galleryImageThumbnail=img_qtgallery_frosch_thumb_jpg}},QtProject{qtProjectId=4,qtProjectName="particles",qtProjectPackage=bin_projects_qt_particles_tar_bz2,qtProjectDescription="Gravitationsimulator von Partikelteilchen mit elektromagnetischer Wechselwirkung",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_particles_jpg,galleryImageThumbnail=img_qtgallery_particles_thumb_jpg}},QtProject{qtProjectId=5,qtProjectName="trainer",qtProjectPackage=bin_projects_qt_trainer_tar_bz2,qtProjectDescription="Trainiert Bundesländern, Vokabeln, etc.",qtProjectGalleryImage=GalleryImage{galleryImageSource=img_qtgallery_trainer_jpg,galleryImageThumbnail=img_qtgallery_trainer_thumb_jpg}}]
getJavaWorkSheets :: [WorkSheet]
getJavaWorkSheets = [ WorkSheet{workSheetNumber=1,workSheetSource=bin_script_java_blatt01_tex,workSheetDocument=bin_script_java_blatt01_pdf},WorkSheet{workSheetNumber=2,workSheetSource=bin_script_java_blatt02_tex,workSheetDocument=bin_script_java_blatt02_pdf},WorkSheet{workSheetNumber=3,workSheetSource=bin_script_java_blatt03_tex,workSheetDocument=bin_script_java_blatt03_pdf},WorkSheet{workSheetNumber=4,workSheetSource=bin_script_java_blatt04_tex,workSheetDocument=bin_script_java_blatt04_pdf},WorkSheet{workSheetNumber=5,workSheetSource=bin_script_java_blatt05_tex,workSheetDocument=bin_script_java_blatt05_pdf},WorkSheet{workSheetNumber=6,workSheetSource=bin_script_java_blatt06_tex,workSheetDocument=bin_script_java_blatt06_pdf},WorkSheet{workSheetNumber=7,workSheetSource=bin_script_java_blatt07_tex,workSheetDocument=bin_script_java_blatt07_pdf},WorkSheet{workSheetNumber=8,workSheetSource=bin_script_java_blatt08_tex,workSheetDocument=bin_script_java_blatt08_pdf},WorkSheet{workSheetNumber=9,workSheetSource=bin_script_java_blatt09_tex,workSheetDocument=bin_script_java_blatt09_pdf},WorkSheet{workSheetNumber=10,workSheetSource=bin_script_java_blatt10_tex,workSheetDocument=bin_script_java_blatt10_pdf},WorkSheet{workSheetNumber=11,workSheetSource=bin_script_java_blatt11_tex,workSheetDocument=bin_script_java_blatt11_pdf},WorkSheet{workSheetNumber=12,workSheetSource=bin_script_java_blatt12_tex,workSheetDocument=bin_script_java_blatt12_pdf},WorkSheet{workSheetNumber=13,workSheetSource=bin_script_java_blatt13_tex,workSheetDocument=bin_script_java_blatt13_pdf}]
getQtWorkSheets :: [WorkSheet]
getQtWorkSheets = [ WorkSheet{workSheetNumber=1,workSheetSource=bin_script_qt_blatt1_tex,workSheetDocument=bin_script_qt_blatt1_pdf},WorkSheet{workSheetNumber=2,workSheetSource=bin_script_qt_blatt2_tex,workSheetDocument=bin_script_qt_blatt2_pdf},WorkSheet{workSheetNumber=3,workSheetSource=bin_script_qt_blatt3_tex,workSheetDocument=bin_script_qt_blatt3_pdf},WorkSheet{workSheetNumber=4,workSheetSource=bin_script_qt_blatt4_tex,workSheetDocument=bin_script_qt_blatt4_pdf},WorkSheet{workSheetNumber=5,workSheetSource=bin_script_qt_blatt5_tex,workSheetDocument=bin_script_qt_blatt5_pdf},WorkSheet{workSheetNumber=6,workSheetSource=bin_script_qt_blatt6_tex,workSheetDocument=bin_script_qt_blatt6_pdf},WorkSheet{workSheetNumber=7,workSheetSource=bin_script_qt_blatt7_tex,workSheetDocument=bin_script_qt_blatt7_pdf}]
getOpenGLQtLessons :: [OpenGLQtLesson]
getOpenGLQtLessons = [ OpenGLQtLesson{openGLQtLessonNumber=1,openGLQtLessonPackage=bin_pkg_nehe_lesson01_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=01"},OpenGLQtLesson{openGLQtLessonNumber=2,openGLQtLessonPackage=bin_pkg_nehe_lesson02_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=02"},OpenGLQtLesson{openGLQtLessonNumber=3,openGLQtLessonPackage=bin_pkg_nehe_lesson03_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=03"},OpenGLQtLesson{openGLQtLessonNumber=4,openGLQtLessonPackage=bin_pkg_nehe_lesson04_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=04"},OpenGLQtLesson{openGLQtLessonNumber=5,openGLQtLessonPackage=bin_pkg_nehe_lesson05_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=05"},OpenGLQtLesson{openGLQtLessonNumber=6,openGLQtLessonPackage=bin_pkg_nehe_lesson06_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=06"},OpenGLQtLesson{openGLQtLessonNumber=7,openGLQtLessonPackage=bin_pkg_nehe_lesson07_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=07"},OpenGLQtLesson{openGLQtLessonNumber=8,openGLQtLessonPackage=bin_pkg_nehe_lesson08_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=08"},OpenGLQtLesson{openGLQtLessonNumber=9,openGLQtLessonPackage=bin_pkg_nehe_lesson09_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=09"},OpenGLQtLesson{openGLQtLessonNumber=11,openGLQtLessonPackage=bin_pkg_nehe_lesson11_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=11"},OpenGLQtLesson{openGLQtLessonNumber=12,openGLQtLessonPackage=bin_pkg_nehe_lesson12_zip,openGLQtLessonLink="http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=12"}]
data BeamerSlide = BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS|BEAMER_SILDE_SPEEDING_UP_GEO_PREFERENCES
	deriving(Show,Eq,Read,Enum,Bounded)
getBeamerSlideWidget :: BeamerSlide -> t -> Markup
getBeamerSlideWidget BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS = $(hamletFile "templates/beamer/slides/combined_pruning_level_of_better_than_graphs.hamlet")
getBeamerSlideWidget BEAMER_SILDE_SPEEDING_UP_GEO_PREFERENCES = $(hamletFile "templates/beamer/slides/speeding_up_geo_preferences.hamlet")
instance ToMarkup TatoebaLanguage where
	toMarkup = toMarkup . getTatoebaLanguageName

instance PathPiece TatoebaLanguage where
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show

instance PathPiece BeamerSlide where
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show

getBeamerSlideTitle :: BeamerSlide -> Text
getBeamerSlideTitle slide = Text.intercalate " " $ List.drop 2 $ map (\word -> Text.cons (Char.toUpper . Text.head $ word) (Text.tail word)) $ Text.words $ Text.toLower $ Text.replace "_" " " $ Text.pack $ show slide
