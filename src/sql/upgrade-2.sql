BEGIN TRANSACTION;

DELETE FROM Workers;
ALTER TABLE Workers ADD COLUMN machine TEXT NOT NULL;

COMMIT;
