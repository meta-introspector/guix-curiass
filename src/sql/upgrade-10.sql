BEGIN TRANSACTION;

ALTER TABLE Evaluations RENAME COLUMN in_progress TO status;

-- Set all pending evaluations to aborted.
UPDATE Evaluations SET status = 2 WHERE status = 1;

-- All evaluations that did not trigger any build are set to failed.
UPDATE Evaluations SET status = 1 WHERE id NOT IN
(SELECT evaluation FROM Builds);

COMMIT;
