{-# LANGUAGE QuasiQuotes #-}
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Database.HDBC
import Database.HDBC.PostgreSQL
import Prelude
import MyQQ 
import MyTools
import System.Environment

createTables :: String
createTables = [literal|
DROP TABLE IF EXISTS tatoeba_links;
DROP TABLE IF EXISTS tatoeba_sentences;
CREATE TABLE IF NOT EXISTS tatoeba_sentences ( 
	sentence_id INT NOT NULL,
	sentence_language VARCHAR(4) NOT NULL, 
	sentence_text TEXT NOT NULL,
	CONSTRAINT sentence_unique UNIQUE (sentence_language, sentence_text),
	CONSTRAINT sentence_primary PRIMARY KEY (sentence_id)
);

CREATE TABLE IF NOT EXISTS tatoeba_links ( 
	link_sentence_id INT NOT NULL, 
	link_translation_id INT NOT NULL, 
	CONSTRAINT link_unique UNIQUE(link_sentence_id, link_translation_id), 
	CONSTRAINT link_sentence_foreign FOREIGN KEY(link_sentence_id) REFERENCES tatoeba_sentences (sentence_id) ON DELETE CASCADE,
	CONSTRAINT link_translation_foreign FOREIGN KEY(link_translation_id) REFERENCES tatoeba_sentences (sentence_id) ON DELETE CASCADE 
);

CREATE RULE "sentence_rule" AS ON INSERT TO tatoeba_sentences
	WHERE EXISTS(SELECT 1 FROM tatoeba_sentences WHERE (sentence_text, sentence_language)=(NEW.sentence_text, NEW.sentence_language)  )
	DO INSTEAD NOTHING;

CREATE RULE "link_rule" AS ON INSERT TO tatoeba_links
	WHERE EXISTS(SELECT 1 FROM tatoeba_links WHERE (link_sentence_id,link_translation_id)=(NEW.link_sentence_id,NEW.link_translation_id)  )
		OR NOT EXISTS(SELECT 1 FROM tatoeba_sentences WHERE sentence_id = NEW.link_sentence_id)
		OR NOT EXISTS(SELECT 1 FROM tatoeba_sentences WHERE sentence_id = NEW.link_translation_id)
	DO INSTEAD NOTHING;
|] 

main :: IO ()
main = do
	args <- getArgs
	connectionString <- getPostgresConnectionString (args!!0) (read $ args!!1)
	dbh <- connectPostgreSQL $ B.toString connectionString
	runRaw dbh createTables 
	commit dbh

	sentenceStmt <- prepare dbh "INSERT INTO tatoeba_sentences (sentence_id, sentence_language, sentence_text) VALUES (?, ?, ?)"
	B.readFile "sentences.csv" >>= (\contents -> executeMany sentenceStmt $ map (\line -> map toSql $ (C.split '\t') line) $ B.lines contents)
	commit dbh

	runRaw dbh "DELETE FROM tatoeba_sentences WHERE CHAR_LENGTH(sentence_language) < 3;"

	linkStmt <- prepare dbh "INSERT INTO tatoeba_links (link_sentence_id, link_translation_id) VALUES (?, ?)"
	B.readFile "links.csv" >>= (\contents -> executeMany linkStmt $ map (\line -> map toSql $ (C.split '\t') line) $ B.lines contents)
	commit dbh

	disconnect dbh
