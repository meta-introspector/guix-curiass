BEGIN TRANSACTION;

ALTER TABLE Specifications ADD COLUMN period INTEGER NOT NULL DEFAULT 0;

COMMIT;