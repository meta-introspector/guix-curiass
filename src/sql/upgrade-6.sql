BEGIN TRANSACTION;

ALTER TABLE Jobs DROP CONSTRAINT fk_builds;
ALTER TABLE Jobs ADD CONSTRAINT fk_builds
   FOREIGN KEY (build)
   REFERENCES Builds(id)
   ON DELETE CASCADE;

COMMIT;
