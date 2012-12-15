
CREATE TABLE IF NOT EXISTS books (
_id SERIAL,
book_name CHARACTER VARYING NOT NULL,
book_language CHARACTER VARYING DEFAULT 'ja',
book_timestamp timestamp with time zone default NOW(),
constraint book_primary PRIMARY KEY(_id),
constraint book_name_unique UNIQUE (book_name)
);
CREATE TABLE IF NOT EXISTS chapters (
_id SERIAL,
chapter_book_id INTEGER NOT NULL,
chapter_volume CHARACTER VARYING NOT NULL,
constraint chapter_primary PRIMARY KEY(_id),
constraint chapter_unique UNIQUE ( chapter_book_id, chapter_volume ),
constraint chapter_foreign FOREIGN KEY (chapter_book_id) references books (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS cards (
_id SERIAL ,
card_script CHARACTER VARYING NOT NULL ,
card_script_comment CHARACTER VARYING ,
card_speech CHARACTER VARYING,
card_speech_comment CHARACTER VARYING ,
card_type INT,
constraint card_primary PRIMARY KEY(_id),
constraint card_unique UNIQUE ( card_speech, card_script, card_script_comment, card_speech_comment)
);


CREATE TABLE IF NOT EXISTS content (
_id SERIAL ,
content_chapter_id INTEGER NOT NULL,
content_card_id INTEGER NOT NULL,
constraint content_primary PRIMARY KEY(_id),
constraint content_unique UNIQUE( content_chapter_id, content_card_id),
constraint content_foreign_chapter foreign key (content_chapter_id) references chapters (_id) ON DELETE CASCADE,
constraint content_foreign_card foreign key (content_card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS translations (
_id SERIAL,
translation_card_id INTEGER NOT NULL,
translation_language CHARACTER VARYING NOT NULL,
translation_content CHARACTER VARYING NOT NULL ,
translation_comment CHARACTER VARYING ,
constraint translation_unique UNIQUE ( translation_card_id, translation_language),
constraint translation_foreign_card foreign key (translation_card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS filing (
_id SERIAL,
filing_card_id INTEGER NOT NULL, 
filing_rank INTEGER NOT NULL, 
filing_session INTEGER DEFAULT 0, 
filing_interval INTEGER DEFAULT 0, 
filing_grades INTEGER DEFAULT 0, 
filing_priority INTEGER DEFAULT 0, 
filing_count INTEGER DEFAULT 0, 
filing_difficulty FLOAT DEFAULT 0, 
filing_sequence INTEGER DEFAULT 12, 
constraint filing_primary PRIMARY KEY(_id), 
constraint filing_unique UNIQUE(filing_card_id, filing_sequence),
constraint filing_foreign_card foreign key (filing_card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS filing_data (
_id SERIAL, 
filing_timestamp INTEGER NOT NULL DEFAULT 0, 
filing_session INTEGER NOT NULL DEFAULT 0, 
filing_sequence INTEGER NOT NULL, 
constraint filing_data_primary  PRIMARY KEY(_id), 
constraint filing_data_unique UNIQUE(filing_sequence)
);
