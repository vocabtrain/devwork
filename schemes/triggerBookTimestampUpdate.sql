CREATE OR REPLACE FUNCTION on_change_chapter_update_book() RETURNS TRIGGER AS 
$BODY$
	DECLARE
		id integer;
	BEGIN
		IF (TG_OP = 'DELETE') THEN
			id := OLD._id;
		ELSE
			id := NEW._id;
		END IF;
		UPDATE books 
			SET book_timestamp = NOW()
			WHERE books._id IN
			(SELECT books._id FROM books
				JOIN chapters ON chapter_book_id = books._id 
				WHERE chapters._id = id);
		RETURN NULL;
	END;
$BODY$ 
	LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION on_change_card_update_book() RETURNS TRIGGER AS 
$BODY$
	DECLARE
		id integer;
	BEGIN
		IF (TG_OP = 'DELETE') THEN
			id := OLD._id;
		ELSE
			id := NEW._id;
		END IF;
		UPDATE books 
			SET book_timestamp = NOW()
			WHERE books._id IN
			(SELECT books._id FROM books
				JOIN chapters ON chapter_book_id = books._id 
				JOIN content ON content_chapter_id = chapters._id 
				JOIN cards ON content_card_id = cards._id
				WHERE cards._id = id);
		RETURN NULL;
	END;
$BODY$ 
	LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION on_change_translation_update_book() RETURNS TRIGGER AS 
$BODY$
	DECLARE
		id integer;
	BEGIN
		IF (TG_OP = 'DELETE') THEN
			id := OLD._id;
		ELSE
			id := NEW._id;
		END IF;
		UPDATE books 
			SET book_timestamp = NOW()
			WHERE books._id IN
			(SELECT books._id FROM books
				JOIN chapters ON chapter_book_id = books._id 
				JOIN content ON content_chapter_id = chapters._id 
				JOIN cards ON content_card_id = cards._id
				JOIN translations ON translation_card_id = cards._id
				WHERE translations._id = id);
		RETURN NULL;
	END;
$BODY$ 
	LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION on_create_book() RETURNS TRIGGER AS 
$BODY$
	BEGIN
		insert into cache_book_translang (book_id, book_language)
			SELECT NEW._id, translation_language from chapters 
				join content on content_chapter_id = chapters._id 
				join cards on content_card_id = cards._id 
				join translations on translation_card_id = cards._id
				where chapter_book_id = NEW._id
				group by translation_language;
		RETURN NULL;
	END;
$BODY$ 
	LANGUAGE plpgsql;

drop trigger if exists trigger_on_change_chapter_update_book on chapters;
drop trigger if exists trigger_on_change_card_update_book on cards;
drop trigger if exists trigger_on_change_translation_update_book on translations;
drop trigger if exists trigger_on_create_book on books;

CREATE TRIGGER trigger_on_create_book AFTER INSERT ON books
	FOR EACH ROW
	EXECUTE PROCEDURE on_create_book();

CREATE TRIGGER trigger_on_change_card_update_book AFTER INSERT OR UPDATE OR DELETE ON cards
	FOR EACH ROW
	EXECUTE PROCEDURE on_change_card_update_book();

CREATE TRIGGER trigger_on_change_chapter_update_book AFTER INSERT OR UPDATE OR DELETE ON chapters
	FOR EACH ROW
	EXECUTE PROCEDURE on_change_chapter_update_book();

CREATE TRIGGER trigger_on_change_translation_update_book AFTER INSERT OR UPDATE OR DELETE ON translations
	FOR EACH ROW
	EXECUTE PROCEDURE on_change_translation_update_book();

-- update cards set card_type = 208 where cards._id = 1;
-- update translations set translation_language = 'deu' where translations._id = 1;
-- update chapters set chapter_volume = 'Unio' where chapters._id = 1;
