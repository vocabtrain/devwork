DROP TABLE IF EXISTS cache_book_translang;
CREATE TABLE IF NOT EXISTS cache_book_translang (
	id SERIAL,
    book_id INT NOT NULL,
    book_language TEXT NOT NULL,
    CONSTRAINT cache_book_foreign_book FOREIGN KEY (book_id) REFERENCES books (_id),
    CONSTRAINT cache_book_unique UNIQUE (book_id, book_language)
);

insert into cache_book_translang (book_id, book_language)
	SELECT books._id, translation_language from books
		join chapters on chapter_book_id = books._id 
		join content on content_chapter_id = chapters._id 
		join cards on content_card_id = cards._id 
		join translations on translation_card_id = cards._id
		group by translation_language, books._id;
