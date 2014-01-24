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
import Widgets

class (Show a, Read a) => BeamerSlide a where
	getBeamerSlideTitleDefault :: a -> Text
	getBeamerSlideTitleDefault slide = Text.intercalate " " $ List.drop 2 $ map (\word -> Text.cons (Char.toUpper . Text.head $ word) (Text.tail word)) $ Text.words $ Text.toLower $ Text.replace "_" " " $ Text.pack $ show slide
	getBeamerSlideTitle :: a -> Text
	getBeamerSlideTitle = getBeamerSlideTitleDefault
	getBeamerSlideWidget :: a -> Widget

instance BeamerSlide BeamerSlidePublic where
	getBeamerSlideWidget BEAMER_SILDE_SPEICHERANALYSE_VON_B_TREES = $(whamletFile "templates/beamer/public/speicheranalyse_von_b_trees.hamlet")
	getBeamerSlideWidget BEAMER_SILDE_TEST = $(whamletFile "templates/beamer/public/test.hamlet")
	getBeamerSlideTitle BEAMER_SILDE_SPEICHERANALYSE_VON_B_TREES = "Speicheranalyse von B-Bäumen"
	getBeamerSlideTitle a = getBeamerSlideTitleDefault a
instance BeamerSlide BeamerSlidePrivate where
	getBeamerSlideWidget BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS = $(whamletFile "templates/beamer/private/combined_pruning_level_of_better_than_graphs.hamlet")
	getBeamerSlideWidget BEAMER_SILDE_CONTAINS_PREFERENCE = $(whamletFile "templates/beamer/private/contains_preference.hamlet")
	getBeamerSlideWidget BEAMER_SILDE_POTENTIAL_GEO_PREFS = $(whamletFile "templates/beamer/private/potential_geo_prefs.hamlet")
	getBeamerSlideWidget BEAMER_SILDE_SPEEDING_UP_GEO_PREFERENCES = $(whamletFile "templates/beamer/private/speeding_up_geo_preferences.hamlet")
	getBeamerSlideTitle BEAMER_SILDE_COMBINED_PRUNING_LEVEL_OF_BETTER_THAN_GRAPHS = "Combined pruning Level of Better-Than-Graphs"
	getBeamerSlideTitle a = getBeamerSlideTitleDefault a
instance ToAppMessage TatoebaLanguage where 
	toAppMessage LANG_ACM = MsgLang_acm
	toAppMessage LANG_AFR = MsgLang_afr
	toAppMessage LANG_AIN = MsgLang_ain
	toAppMessage LANG_ANG = MsgLang_ang
	toAppMessage LANG_ARA = MsgLang_ara
	toAppMessage LANG_ARQ = MsgLang_arq
	toAppMessage LANG_ARZ = MsgLang_arz
	toAppMessage LANG_AST = MsgLang_ast
	toAppMessage LANG_AVK = MsgLang_avk
	toAppMessage LANG_AZE = MsgLang_aze
	toAppMessage LANG_BEL = MsgLang_bel
	toAppMessage LANG_BEN = MsgLang_ben
	toAppMessage LANG_BER = MsgLang_ber
	toAppMessage LANG_BOD = MsgLang_bod
	toAppMessage LANG_BOS = MsgLang_bos
	toAppMessage LANG_BRE = MsgLang_bre
	toAppMessage LANG_BUL = MsgLang_bul
	toAppMessage LANG_CAT = MsgLang_cat
	toAppMessage LANG_CES = MsgLang_ces
	toAppMessage LANG_CHA = MsgLang_cha
	toAppMessage LANG_CKT = MsgLang_ckt
	toAppMessage LANG_CMN = MsgLang_cmn
	toAppMessage LANG_COR = MsgLang_cor
	toAppMessage LANG_CYCL = MsgLang_cycl
	toAppMessage LANG_CYM = MsgLang_cym
	toAppMessage LANG_DAN = MsgLang_dan
	toAppMessage LANG_DEU = MsgLang_deu
	toAppMessage LANG_DSB = MsgLang_dsb
	toAppMessage LANG_ELL = MsgLang_ell
	toAppMessage LANG_ENG = MsgLang_eng
	toAppMessage LANG_EPO = MsgLang_epo
	toAppMessage LANG_EST = MsgLang_est
	toAppMessage LANG_EUS = MsgLang_eus
	toAppMessage LANG_EWE = MsgLang_ewe
	toAppMessage LANG_FAO = MsgLang_fao
	toAppMessage LANG_FIN = MsgLang_fin
	toAppMessage LANG_FRA = MsgLang_fra
	toAppMessage LANG_FRY = MsgLang_fry
	toAppMessage LANG_GLA = MsgLang_gla
	toAppMessage LANG_GLE = MsgLang_gle
	toAppMessage LANG_GLG = MsgLang_glg
	toAppMessage LANG_GRC = MsgLang_grc
	toAppMessage LANG_GRN = MsgLang_grn
	toAppMessage LANG_HEB = MsgLang_heb
	toAppMessage LANG_HIL = MsgLang_hil
	toAppMessage LANG_HIN = MsgLang_hin
	toAppMessage LANG_HRV = MsgLang_hrv
	toAppMessage LANG_HSB = MsgLang_hsb
	toAppMessage LANG_HUN = MsgLang_hun
	toAppMessage LANG_HYE = MsgLang_hye
	toAppMessage LANG_IDO = MsgLang_ido
	toAppMessage LANG_ILE = MsgLang_ile
	toAppMessage LANG_INA = MsgLang_ina
	toAppMessage LANG_IND = MsgLang_ind
	toAppMessage LANG_ISL = MsgLang_isl
	toAppMessage LANG_ITA = MsgLang_ita
	toAppMessage LANG_JBO = MsgLang_jbo
	toAppMessage LANG_JPN = MsgLang_jpn
	toAppMessage LANG_KAT = MsgLang_kat
	toAppMessage LANG_KAZ = MsgLang_kaz
	toAppMessage LANG_KHM = MsgLang_khm
	toAppMessage LANG_KOR = MsgLang_kor
	toAppMessage LANG_KSH = MsgLang_ksh
	toAppMessage LANG_KUR = MsgLang_kur
	toAppMessage LANG_LAD = MsgLang_lad
	toAppMessage LANG_LAO = MsgLang_lao
	toAppMessage LANG_LAT = MsgLang_lat
	toAppMessage LANG_LIT = MsgLang_lit
	toAppMessage LANG_LLD = MsgLang_lld
	toAppMessage LANG_LVS = MsgLang_lvs
	toAppMessage LANG_LZH = MsgLang_lzh
	toAppMessage LANG_MAL = MsgLang_mal
	toAppMessage LANG_MAR = MsgLang_mar
	toAppMessage LANG_MLG = MsgLang_mlg
	toAppMessage LANG_MLT = MsgLang_mlt
	toAppMessage LANG_MON = MsgLang_mon
	toAppMessage LANG_MRI = MsgLang_mri
	toAppMessage LANG_NAN = MsgLang_nan
	toAppMessage LANG_NDS = MsgLang_nds
	toAppMessage LANG_NLD = MsgLang_nld
	toAppMessage LANG_NOB = MsgLang_nob
	toAppMessage LANG_NON = MsgLang_non
	toAppMessage LANG_NOV = MsgLang_nov
	toAppMessage LANG_NPI = MsgLang_npi
	toAppMessage LANG_OCI = MsgLang_oci
	toAppMessage LANG_ORV = MsgLang_orv
	toAppMessage LANG_OSS = MsgLang_oss
	toAppMessage LANG_PCD = MsgLang_pcd
	toAppMessage LANG_PES = MsgLang_pes
	toAppMessage LANG_PMS = MsgLang_pms
	toAppMessage LANG_PNB = MsgLang_pnb
	toAppMessage LANG_POL = MsgLang_pol
	toAppMessage LANG_POR = MsgLang_por
	toAppMessage LANG_PRG = MsgLang_prg
	toAppMessage LANG_QUE = MsgLang_que
	toAppMessage LANG_QYA = MsgLang_qya
	toAppMessage LANG_ROH = MsgLang_roh
	toAppMessage LANG_RON = MsgLang_ron
	toAppMessage LANG_RUS = MsgLang_rus
	toAppMessage LANG_SAN = MsgLang_san
	toAppMessage LANG_SCN = MsgLang_scn
	toAppMessage LANG_SJN = MsgLang_sjn
	toAppMessage LANG_SLK = MsgLang_slk
	toAppMessage LANG_SLV = MsgLang_slv
	toAppMessage LANG_SPA = MsgLang_spa
	toAppMessage LANG_SQI = MsgLang_sqi
	toAppMessage LANG_SRP = MsgLang_srp
	toAppMessage LANG_SWE = MsgLang_swe
	toAppMessage LANG_SWH = MsgLang_swh
	toAppMessage LANG_TAT = MsgLang_tat
	toAppMessage LANG_TEL = MsgLang_tel
	toAppMessage LANG_TGK = MsgLang_tgk
	toAppMessage LANG_TGL = MsgLang_tgl
	toAppMessage LANG_THA = MsgLang_tha
	toAppMessage LANG_TLH = MsgLang_tlh
	toAppMessage LANG_TOKI = MsgLang_toki
	toAppMessage LANG_TPI = MsgLang_tpi
	toAppMessage LANG_TPW = MsgLang_tpw
	toAppMessage LANG_TUR = MsgLang_tur
	toAppMessage LANG_UIG = MsgLang_uig
	toAppMessage LANG_UKR = MsgLang_ukr
	toAppMessage LANG_UND = MsgLang_und
	toAppMessage LANG_URD = MsgLang_urd
	toAppMessage LANG_UZB = MsgLang_uzb
	toAppMessage LANG_VIE = MsgLang_vie
	toAppMessage LANG_VOL = MsgLang_vol
	toAppMessage LANG_WUU = MsgLang_wuu
	toAppMessage LANG_XAL = MsgLang_xal
	toAppMessage LANG_XHO = MsgLang_xho
	toAppMessage LANG_YID = MsgLang_yid
	toAppMessage LANG_YUE = MsgLang_yue
	toAppMessage LANG_ZSM = MsgLang_zsm
