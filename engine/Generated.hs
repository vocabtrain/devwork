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

class ShowText a where
	showText :: a -> Text

class (Show a, Read a) => BeamerSlide a where
	getBeamerSlideTitle :: a -> Text
	getBeamerSlideTitle slide = Text.intercalate " " $ List.drop 2 $ map (\word -> Text.cons (Char.toUpper . Text.head $ word) (Text.tail word)) $ Text.words $ Text.toLower $ Text.replace "_" " " $ Text.pack $ show slide
	getBeamerSlideWidget :: a -> t -> Markup

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
data TatoebaLanguage = LANG_ACM
	 | LANG_AFR
	 | LANG_AIN
	 | LANG_ANG
	 | LANG_ARA
	 | LANG_ARZ
	 | LANG_AST
	 | LANG_BEL
	 | LANG_BEN
	 | LANG_BER
	 | LANG_BOS
	 | LANG_BRE
	 | LANG_BUL
	 | LANG_CAT
	 | LANG_CES
	 | LANG_CHA
	 | LANG_CMN
	 | LANG_CYCL
	 | LANG_CYM
	 | LANG_DAN
	 | LANG_DEU
	 | LANG_DSB
	 | LANG_ELL
	 | LANG_ENG
	 | LANG_EPO
	 | LANG_EST
	 | LANG_EUS
	 | LANG_EWE
	 | LANG_FAO
	 | LANG_FIN
	 | LANG_FRA
	 | LANG_FRY
	 | LANG_GLA
	 | LANG_GLE
	 | LANG_GLG
	 | LANG_GRN
	 | LANG_HEB
	 | LANG_HIN
	 | LANG_HRV
	 | LANG_HSB
	 | LANG_HUN
	 | LANG_HYE
	 | LANG_IDO
	 | LANG_ILE
	 | LANG_INA
	 | LANG_IND
	 | LANG_ISL
	 | LANG_ITA
	 | LANG_JBO
	 | LANG_JPN
	 | LANG_KAT
	 | LANG_KAZ
	 | LANG_KOR
	 | LANG_KSH
	 | LANG_KUR
	 | LANG_LAD
	 | LANG_LAT
	 | LANG_LIT
	 | LANG_LLD
	 | LANG_LVS
	 | LANG_LZH
	 | LANG_MAL
	 | LANG_MLG
	 | LANG_MON
	 | LANG_MRI
	 | LANG_NAN
	 | LANG_NDS
	 | LANG_NLD
	 | LANG_NOB
	 | LANG_NON
	 | LANG_NOV
	 | LANG_OCI
	 | LANG_ORV
	 | LANG_OSS
	 | LANG_PES
	 | LANG_PMS
	 | LANG_PNB
	 | LANG_POL
	 | LANG_POR
	 | LANG_QUE
	 | LANG_QYA
	 | LANG_ROH
	 | LANG_RON
	 | LANG_RUS
	 | LANG_SAN
	 | LANG_SCN
	 | LANG_SJN
	 | LANG_SLK
	 | LANG_SLV
	 | LANG_SPA
	 | LANG_SQI
	 | LANG_SRP
	 | LANG_SWE
	 | LANG_SWH
	 | LANG_TAT
	 | LANG_TEL
	 | LANG_TGL
	 | LANG_THA
	 | LANG_TLH
	 | LANG_TOKI
	 | LANG_TPI
	 | LANG_TUR
	 | LANG_UIG
	 | LANG_UKR
	 | LANG_UND
	 | LANG_URD
	 | LANG_UZB
	 | LANG_VIE
	 | LANG_VOL
	 | LANG_WUU
	 | LANG_XAL
	 | LANG_XHO
	 | LANG_YID
	 | LANG_YUE
	 | LANG_ZSM
	deriving(Eq,Enum,Bounded)
instance Read TatoebaLanguage where 
	readPrec = choice $ strValMap [
		("acm",LANG_ACM)
		, ("afr",LANG_AFR)
		, ("ain",LANG_AIN)
		, ("ang",LANG_ANG)
		, ("ara",LANG_ARA)
		, ("arz",LANG_ARZ)
		, ("ast",LANG_AST)
		, ("bel",LANG_BEL)
		, ("ben",LANG_BEN)
		, ("ber",LANG_BER)
		, ("bos",LANG_BOS)
		, ("bre",LANG_BRE)
		, ("bul",LANG_BUL)
		, ("cat",LANG_CAT)
		, ("ces",LANG_CES)
		, ("cha",LANG_CHA)
		, ("cmn",LANG_CMN)
		, ("cycl",LANG_CYCL)
		, ("cym",LANG_CYM)
		, ("dan",LANG_DAN)
		, ("deu",LANG_DEU)
		, ("dsb",LANG_DSB)
		, ("ell",LANG_ELL)
		, ("eng",LANG_ENG)
		, ("epo",LANG_EPO)
		, ("est",LANG_EST)
		, ("eus",LANG_EUS)
		, ("ewe",LANG_EWE)
		, ("fao",LANG_FAO)
		, ("fin",LANG_FIN)
		, ("fra",LANG_FRA)
		, ("fry",LANG_FRY)
		, ("gla",LANG_GLA)
		, ("gle",LANG_GLE)
		, ("glg",LANG_GLG)
		, ("grn",LANG_GRN)
		, ("heb",LANG_HEB)
		, ("hin",LANG_HIN)
		, ("hrv",LANG_HRV)
		, ("hsb",LANG_HSB)
		, ("hun",LANG_HUN)
		, ("hye",LANG_HYE)
		, ("ido",LANG_IDO)
		, ("ile",LANG_ILE)
		, ("ina",LANG_INA)
		, ("ind",LANG_IND)
		, ("isl",LANG_ISL)
		, ("ita",LANG_ITA)
		, ("jbo",LANG_JBO)
		, ("jpn",LANG_JPN)
		, ("kat",LANG_KAT)
		, ("kaz",LANG_KAZ)
		, ("kor",LANG_KOR)
		, ("ksh",LANG_KSH)
		, ("kur",LANG_KUR)
		, ("lad",LANG_LAD)
		, ("lat",LANG_LAT)
		, ("lit",LANG_LIT)
		, ("lld",LANG_LLD)
		, ("lvs",LANG_LVS)
		, ("lzh",LANG_LZH)
		, ("mal",LANG_MAL)
		, ("mlg",LANG_MLG)
		, ("mon",LANG_MON)
		, ("mri",LANG_MRI)
		, ("nan",LANG_NAN)
		, ("nds",LANG_NDS)
		, ("nld",LANG_NLD)
		, ("nob",LANG_NOB)
		, ("non",LANG_NON)
		, ("nov",LANG_NOV)
		, ("oci",LANG_OCI)
		, ("orv",LANG_ORV)
		, ("oss",LANG_OSS)
		, ("pes",LANG_PES)
		, ("pms",LANG_PMS)
		, ("pnb",LANG_PNB)
		, ("pol",LANG_POL)
		, ("por",LANG_POR)
		, ("que",LANG_QUE)
		, ("qya",LANG_QYA)
		, ("roh",LANG_ROH)
		, ("ron",LANG_RON)
		, ("rus",LANG_RUS)
		, ("san",LANG_SAN)
		, ("scn",LANG_SCN)
		, ("sjn",LANG_SJN)
		, ("slk",LANG_SLK)
		, ("slv",LANG_SLV)
		, ("spa",LANG_SPA)
		, ("sqi",LANG_SQI)
		, ("srp",LANG_SRP)
		, ("swe",LANG_SWE)
		, ("swh",LANG_SWH)
		, ("tat",LANG_TAT)
		, ("tel",LANG_TEL)
		, ("tgl",LANG_TGL)
		, ("tha",LANG_THA)
		, ("tlh",LANG_TLH)
		, ("toki",LANG_TOKI)
		, ("tpi",LANG_TPI)
		, ("tur",LANG_TUR)
		, ("uig",LANG_UIG)
		, ("ukr",LANG_UKR)
		, ("und",LANG_UND)
		, ("urd",LANG_URD)
		, ("uzb",LANG_UZB)
		, ("vie",LANG_VIE)
		, ("vol",LANG_VOL)
		, ("wuu",LANG_WUU)
		, ("xal",LANG_XAL)
		, ("xho",LANG_XHO)
		, ("yid",LANG_YID)
		, ("yue",LANG_YUE)
		, ("zsm",LANG_ZSM)
		]
		where
			strValMap :: [(String, TatoebaLanguage)] -> [ReadPrec TatoebaLanguage]
			strValMap = map (\(x, y) -> lift $ string x >> return y)
instance Show TatoebaLanguage where
	show(LANG_ACM)="acm"
	show(LANG_AFR)="afr"
	show(LANG_AIN)="ain"
	show(LANG_ANG)="ang"
	show(LANG_ARA)="ara"
	show(LANG_ARZ)="arz"
	show(LANG_AST)="ast"
	show(LANG_BEL)="bel"
	show(LANG_BEN)="ben"
	show(LANG_BER)="ber"
	show(LANG_BOS)="bos"
	show(LANG_BRE)="bre"
	show(LANG_BUL)="bul"
	show(LANG_CAT)="cat"
	show(LANG_CES)="ces"
	show(LANG_CHA)="cha"
	show(LANG_CMN)="cmn"
	show(LANG_CYCL)="cycl"
	show(LANG_CYM)="cym"
	show(LANG_DAN)="dan"
	show(LANG_DEU)="deu"
	show(LANG_DSB)="dsb"
	show(LANG_ELL)="ell"
	show(LANG_ENG)="eng"
	show(LANG_EPO)="epo"
	show(LANG_EST)="est"
	show(LANG_EUS)="eus"
	show(LANG_EWE)="ewe"
	show(LANG_FAO)="fao"
	show(LANG_FIN)="fin"
	show(LANG_FRA)="fra"
	show(LANG_FRY)="fry"
	show(LANG_GLA)="gla"
	show(LANG_GLE)="gle"
	show(LANG_GLG)="glg"
	show(LANG_GRN)="grn"
	show(LANG_HEB)="heb"
	show(LANG_HIN)="hin"
	show(LANG_HRV)="hrv"
	show(LANG_HSB)="hsb"
	show(LANG_HUN)="hun"
	show(LANG_HYE)="hye"
	show(LANG_IDO)="ido"
	show(LANG_ILE)="ile"
	show(LANG_INA)="ina"
	show(LANG_IND)="ind"
	show(LANG_ISL)="isl"
	show(LANG_ITA)="ita"
	show(LANG_JBO)="jbo"
	show(LANG_JPN)="jpn"
	show(LANG_KAT)="kat"
	show(LANG_KAZ)="kaz"
	show(LANG_KOR)="kor"
	show(LANG_KSH)="ksh"
	show(LANG_KUR)="kur"
	show(LANG_LAD)="lad"
	show(LANG_LAT)="lat"
	show(LANG_LIT)="lit"
	show(LANG_LLD)="lld"
	show(LANG_LVS)="lvs"
	show(LANG_LZH)="lzh"
	show(LANG_MAL)="mal"
	show(LANG_MLG)="mlg"
	show(LANG_MON)="mon"
	show(LANG_MRI)="mri"
	show(LANG_NAN)="nan"
	show(LANG_NDS)="nds"
	show(LANG_NLD)="nld"
	show(LANG_NOB)="nob"
	show(LANG_NON)="non"
	show(LANG_NOV)="nov"
	show(LANG_OCI)="oci"
	show(LANG_ORV)="orv"
	show(LANG_OSS)="oss"
	show(LANG_PES)="pes"
	show(LANG_PMS)="pms"
	show(LANG_PNB)="pnb"
	show(LANG_POL)="pol"
	show(LANG_POR)="por"
	show(LANG_QUE)="que"
	show(LANG_QYA)="qya"
	show(LANG_ROH)="roh"
	show(LANG_RON)="ron"
	show(LANG_RUS)="rus"
	show(LANG_SAN)="san"
	show(LANG_SCN)="scn"
	show(LANG_SJN)="sjn"
	show(LANG_SLK)="slk"
	show(LANG_SLV)="slv"
	show(LANG_SPA)="spa"
	show(LANG_SQI)="sqi"
	show(LANG_SRP)="srp"
	show(LANG_SWE)="swe"
	show(LANG_SWH)="swh"
	show(LANG_TAT)="tat"
	show(LANG_TEL)="tel"
	show(LANG_TGL)="tgl"
	show(LANG_THA)="tha"
	show(LANG_TLH)="tlh"
	show(LANG_TOKI)="toki"
	show(LANG_TPI)="tpi"
	show(LANG_TUR)="tur"
	show(LANG_UIG)="uig"
	show(LANG_UKR)="ukr"
	show(LANG_UND)="und"
	show(LANG_URD)="urd"
	show(LANG_UZB)="uzb"
	show(LANG_VIE)="vie"
	show(LANG_VOL)="vol"
	show(LANG_WUU)="wuu"
	show(LANG_XAL)="xal"
	show(LANG_XHO)="xho"
	show(LANG_YID)="yid"
	show(LANG_YUE)="yue"
	show(LANG_ZSM)="zsm"
derivePersistField "TatoebaLanguage"
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
data BeamerSlidePublic = BEAMER_SILDE_TEST
	deriving(Show,Eq,Read,Enum,Bounded)
instance BeamerSlide BeamerSlidePublic where
	getBeamerSlideWidget BEAMER_SILDE_TEST = $(hamletFile "templates/beamer/public/test.hamlet")
data BeamerSlidePrivate = BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS|BEAMER_SILDE_SPEEDING_UP_GEO_PREFERENCES
	deriving(Show,Eq,Read,Enum,Bounded)
instance BeamerSlide BeamerSlidePrivate where
	getBeamerSlideWidget BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS = $(hamletFile "templates/beamer/private/combined_pruning_level_of_better_than_graphs.hamlet")
	getBeamerSlideWidget BEAMER_SILDE_SPEEDING_UP_GEO_PREFERENCES = $(hamletFile "templates/beamer/private/speeding_up_geo_preferences.hamlet")
instance PathPiece TatoebaLanguage where
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show
instance PathPiece BeamerSlidePrivate where
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show
instance PathPiece BeamerSlidePublic where
	fromPathPiece text = case reads $ Text.unpack text of
		[(x, "")] -> Just x
		_ -> Nothing
	toPathPiece = toPathPiece . Text.pack . show

