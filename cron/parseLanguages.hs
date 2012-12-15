{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Prelude
import System.IO 
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

mapLine :: [C.ByteString] -> [(C.ByteString,C.ByteString)]
mapLine cols 
	| length(cols) /= 5 = []
	| otherwise = [(cols !! 0, cols !! 3)] ++ (if C.length(cols !! 1) > 0 then [(cols !! 1, cols !! 3)] else [])
	

main :: IO ()
main = do
	args <- getArgs
	contents <- B.readFile "ISO-639-2_utf-8.txt" 
	let langMap = Map.fromList $ concat $ map (\line -> mapLine $ (C.split '|') line) $ B.lines contents

	dbh <- connectPostgreSQL $ "host=localhost dbname=" ++ (args!!0) ++ " user=postgres"
	langs <- quickQuery' dbh "SELECT sentence_language FROM tatoeba_sentences GROUP BY sentence_language" []
	
	forM_ langs (\lang -> do
		let langId = C.pack $ fromSql $ lang !! 0
		let langStr = Map.lookup langId langMap
		putStr $ B.toString $ B.concat [langId, "@"::C.ByteString, (maybe langId Prelude.id langStr) ]
		putStr "\n"
		)
	putStr "\n"
	disconnect dbh
