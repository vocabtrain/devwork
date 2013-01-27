{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Prelude
import System.IO 
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Environment

languagesWithStemmer :: [String]
languagesWithStemmer = [
	"deu" ,
	"spa" ,
	"fra" ,
	"nld" ,
	"por" ,
	"rus" ,
	"fin" ,
	"ita" ,
	"tur" ,
	"swe" ,
	"eng" ,
	"tur" 
	]

languagesWithCJK :: [String]
languagesWithCJK = [
	"kor",
	"cmn",
	"wuu",
	"jpn",
	"yue"
	]

genVocabtrainLang (SqlByteString langB) =
				"source source_vocabtrain_" ++ lang ++ " : default {\n" ++
				"\tsql_query = SELECT DISTINCT cards._id, card_script, card_speech FROM cards JOIN content ON content_card_id = cards._id JOIN chapters ON content_chapter_id = chapters._id JOIN books on books._id = chapter_book_id WHERE book_language = '" ++ lang ++ "';\n" ++
				"}\n" ++
				"index vocabtrain_" ++ lang ++ " : " ++
				( if elem lang languagesWithCJK
					then "cjk_common_index" 
					else "common_index" ) ++ " {\n" ++
				"\ttype = plain\n" ++
				"\tsource = source_vocabtrain_" ++ lang ++ "\n" ++
				"\tpath = /home/niki/data/sphinx/vocabtrain/" ++ lang ++ "\n" ++
				"\tenable_star = true\n" ++
				"\tmin_prefix_len = 3\n" ++
				"}\n\n"
					where lang = B.toString langB

genLang (SqlByteString langB) =
				"source source_" ++ lang ++ " : default {\n" ++
				"\tsql_query = SELECT sentence_id, sentence_text FROM tatoeba_sentences WHERE sentence_language = '" ++ lang ++ "';\n" ++
				"}\n" ++
				"index " ++ lang ++ " : " ++
				( if elem lang languagesWithCJK
					then "cjk_common_index" 
					else "common_index" ) ++ " {\n" ++
				"\ttype = plain\n" ++
				"\tsource = source_" ++ lang ++ "\n" ++
				"\tpath = /home/niki/data/sphinx/" ++ lang ++ "\n" ++
				( if elem lang languagesWithStemmer then 
					"\tmorphology = libstemmer_" ++ lang ++ "\n\tmin_stemming_len=4\n" 
					else "") ++
				"}\n\n"
					where lang = B.toString langB
main :: IO ()
main = do
	args <- getArgs
	dbh <- connectPostgreSQL $ "host=localhost dbname=" ++ (args!!0) ++ " user=postgres"
	langs <- quickQuery' dbh "SELECT sentence_language FROM tatoeba_sentences GROUP BY sentence_language" []
	forM_ langs $ \lang -> do
		putStr $ genLang (lang !! 0) 
		putStr $ genVocabtrainLang (lang !! 0) 
	putStr $ "index und : common_index {\n" ++
		"type = distributed\n"
	forM_ langs (\lang -> putStr $ "\t local = " ++ ( fromSql (lang !! 0) ) ++ "\n")
	putStr "}\n"
	putStr $ "index vocabtrain_und : common_index {\n" ++
		"type = distributed\n"
	forM_ langs (\lang -> putStr $ "\t local = vocabtrain_" ++ ( fromSql (lang !! 0) ) ++ "\n")
	putStr "}\n"
	disconnect dbh
