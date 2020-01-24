BEGIN TRANSACTION;

ALTER TABLE Builds RENAME TO OldBuilds;

CREATE TABLE Builds (
  id            INTEGER NOT NULL PRIMARY KEY,
  derivation    TEXT NOT NULL UNIQUE,
  evaluation    INTEGER NOT NULL,
  job_name      TEXT NOT NULL,
  system        TEXT NOT NULL,
  nix_name      TEXT NOT NULL,
  log           TEXT NOT NULL,
  status        INTEGER NOT NULL,
  timestamp     INTEGER NOT NULL,
  starttime     INTEGER NOT NULL,
  stoptime      INTEGER NOT NULL,
  FOREIGN KEY (evaluation) REFERENCES Evaluations (id)
);

INSERT INTO Builds(
  id,
  derivation,
  evaluation,
  job_name,
  system,
  nix_name,
  log,
  status,
  timestamp,
  starttime,
  stoptime
) SELECT rowid,
         derivation,
         evaluation,
         job_name,
         system,
         nix_name,
         log,
         status,
         timestamp,
         starttime,
         stoptime
  FROM OldBuilds;

DROP TABLE OldBuilds;

COMMIT;
