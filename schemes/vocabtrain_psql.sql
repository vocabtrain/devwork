DROP TABLE IF EXISTS translations CASCADE;
DROP TABLE IF EXISTS content CASCADE;
DROP TABLE IF EXISTS cards CASCADE;
DROP TABLE IF EXISTS chapters CASCADE;
DROP TABLE IF EXISTS books CASCADE;
DROP INDEX IF EXISTS book_pkey CASCADE;
DROP TABLE IF EXISTS cache_book_translang CASCADE;

CREATE TABLE IF NOT EXISTS benutzer (
id SERIAL,
email VARCHAR,
nick VARCHAR,
password VARCHAR,
auth_token VARCHAR,
constraint benutzer_pkey PRIMARY KEY(nick),
constraint unique_benutzer UNIQUE (email)
);

CREATE TABLE IF NOT EXISTS books (
_id SERIAL,
book_name CHARACTER VARYING NOT NULL,
book_language CHARACTER VARYING DEFAULT 'ja',
book_timestamp timestamp with time zone NOT NULL default NOW(),
constraint book_pkey PRIMARY KEY(_id),
constraint book_name_unique UNIQUE (book_name)
);
CREATE TABLE IF NOT EXISTS chapters (
_id SERIAL,
chapter_book_id INTEGER NOT NULL,
chapter_volume CHARACTER VARYING NOT NULL,
constraint chapter_pkey PRIMARY KEY(_id),
constraint chapter_unique UNIQUE ( chapter_book_id, chapter_volume ),
constraint chapters_chapter_book_id_fkey FOREIGN KEY (chapter_book_id) references books (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS cards (
_id SERIAL ,
card_script CHARACTER VARYING NOT NULL ,
card_script_comment CHARACTER VARYING ,
card_speech CHARACTER VARYING,
card_speech_comment CHARACTER VARYING ,
card_type INT,
constraint card_pkey PRIMARY KEY(_id),
constraint card_unique UNIQUE ( card_speech, card_script, card_script_comment, card_speech_comment)
);


CREATE TABLE IF NOT EXISTS content (
_id SERIAL ,
content_chapter_id INTEGER NOT NULL,
content_card_id INTEGER NOT NULL,
constraint content_pkey PRIMARY KEY(_id),
constraint unique_content UNIQUE( content_chapter_id, content_card_id),
constraint content_content_chapter_id_fkey foreign key (content_chapter_id) references chapters (_id) ON DELETE CASCADE,
constraint content_content_card_id_fkey foreign key (content_card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS translations (
_id SERIAL,
translation_card_id INTEGER NOT NULL,
translation_language CHARACTER VARYING NOT NULL,
translation_content CHARACTER VARYING NOT NULL ,
translation_comment CHARACTER VARYING ,
constraint translation_pkey PRIMARY KEY(_id),
constraint unique_translation UNIQUE ( translation_card_id, translation_language),
constraint translations_translation_card_id_fkey foreign key (translation_card_id) references cards (_id) ON DELETE CASCADE
);
