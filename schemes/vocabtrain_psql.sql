
CREATE TABLE IF NOT EXISTS books (
_id SERIAL,
book_name TEXT NOT NULL,
book_language TEXT DEFAULT 'ja',
book_timestamp timestamp with time zone default NOW(),
constraint book_primary PRIMARY KEY(_id),
constraint book_name_unique UNIQUE (book_name)
);
CREATE TABLE IF NOT EXISTS chapters (
_id SERIAL,
chapter_book_id INTEGER NOT NULL,
chapter_volume TEXT NOT NULL,
constraint chapter_primary PRIMARY KEY(_id),
constraint chapter_unique UNIQUE ( chapter_book_id, chapter_volume ),
constraint chapter_foreign FOREIGN KEY (chapter_book_id) references books (_id)
);

CREATE TABLE IF NOT EXISTS cards (
_id SERIAL ,
card_script TEXT NOT NULL ,
card_script_comment TEXT ,
card_speech TEXT,
card_speech_comment TEXT ,
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
constraint content_foreign_chapter foreign key (content_chapter_id) references chapters (_id),
constraint content_foreign_card foreign key (content_card_id) references cards (_id)
);

CREATE TABLE IF NOT EXISTS translations (
translation_card_id SERIAL,
translation_language TEXT NOT NULL,
translation_content TEXT NOT NULL ,
translation_comment TEXT ,
constraint translation_unique UNIQUE ( translation_card_id, translation_language),
constraint translation_foreign_card foreign key (translation_card_id) references cards (_id)
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
constraint filing_unique UNIQUE(filing_card_id, filing_sequence)
);

CREATE TABLE IF NOT EXISTS filing_data (_id SERIAL, 
filing_timestamp INTEGER NOT NULL DEFAULT 0, 
filing_session INTEGER NOT NULL DEFAULT 0, 
filing_sequence INTEGER NOT NULL, 
constraint filing_data_primary  PRIMARY KEY(_id), 
constraint filing_data_unique UNIQUE(filing_sequence)
);
