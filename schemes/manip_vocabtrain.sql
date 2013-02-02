DROP TABLE IF EXISTS vocab_book_manip CASCADE;
DROP TABLE IF EXISTS vocab_chapter_manip CASCADE;
DROP TABLE IF EXISTS vocab_card_manip CASCADE;
DROP TABLE IF EXISTS vocab_translation_manip CASCADE;

CREATE TABLE IF NOT EXISTS vocab_book_manip(
	id SERIAL,
	user_id INTEGER NOT NULL, 
	book_id INTEGER NOT NULL,
	"type" VARCHAR(1) NOT NULL,
	timestamp timestamp with time zone default NOW(),
	content TEXT,
	constraint book_manip_primary PRIMARY KEY(id),
	constraint book_manip_foreign_user foreign key (user_id) references "user" (id),
	constraint book_manip_foreign_book foreign key (book_id) references books (_id)
);

CREATE TABLE IF NOT EXISTS vocab_chapter_manip(
	id SERIAL,
	user_id INTEGER NOT NULL, 
	chapter_id INTEGER NOT NULL,
	"type" VARCHAR(1) NOT NULL,
	timestamp timestamp with time zone default NOW(),
	content TEXT,
	constraint chapter_manip_primary PRIMARY KEY(id),
	constraint chapter_manip_foreign_user foreign key (user_id) references "user" (id),
	constraint chapter_manip_foreign_chapter foreign key (chapter_id) references chapters (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS vocab_card_manip(
	id SERIAL,
	user_id INTEGER NOT NULL, 
	card_id INTEGER NOT NULL,
	"type" VARCHAR(1) NOT NULL,
	timestamp timestamp with time zone default NOW(),
	content TEXT,
	constraint card_manip_primary PRIMARY KEY(id),
	constraint card_manip_foreign_user foreign key (user_id) references "user" (id),
	constraint card_manip_foreign_card foreign key (card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS vocab_translation_manip(
	id SERIAL,
	user_id INTEGER NOT NULL, 
	translation_id INTEGER NOT NULL,
	"type" VARCHAR(1) NOT NULL,
	timestamp timestamp with time zone default NOW(),
	content TEXT,
	constraint translation_manip_primary PRIMARY KEY(id),
	constraint translation_manip_foreign_user foreign key (user_id) references "user" (id),
	constraint translation_manip_foreign_translation foreign key (translation_id) references translations (_id) ON DELETE CASCADE
);


