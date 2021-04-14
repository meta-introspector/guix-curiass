BEGIN TRANSACTION;

ALTER TABLE Jobs ADD COLUMN status INTEGER NOT NULL DEFAULT 0;
CREATE INDEX Jobs_system_status ON Jobs (system, status);
UPDATE Jobs SET status = b.status FROM
(SELECT Builds.id, Builds.status FROM Jobs
JOIN Builds ON Jobs.build = Builds.id) b
WHERE Jobs.build = b.id;

COMMIT;
