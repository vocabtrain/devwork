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
constraint filing_filing_card_id_fkey foreign key (filing_card_id) references cards (_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS filing_data (
_id SERIAL, 
filing_timestamp INTEGER NOT NULL DEFAULT 0, 
filing_session INTEGER NOT NULL DEFAULT 0, 
filing_sequence INTEGER NOT NULL, 
constraint filing_data_primary  PRIMARY KEY(_id), 
constraint filing_data_unique UNIQUE(filing_sequence)
);


