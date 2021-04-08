BEGIN TRANSACTION;

ALTER TABLE Jobs ADD COLUMN build INTEGER;
ALTER TABLE Jobs ADD CONSTRAINT fk_builds
   FOREIGN KEY (build)
   REFERENCES Builds(id);
UPDATE Jobs SET build = b.id FROM (SELECT Jobs.evaluation, Jobs.derivation, Builds.id FROM jobs JOIN Builds ON Jobs.derivation = Builds.derivation) b WHERE jobs.evaluation = b.evaluation and jobs.derivation = b.derivation;
ALTER TABLE Jobs DROP COLUMN derivation;

COMMIT;
