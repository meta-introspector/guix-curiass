BEGIN TRANSACTION;

DROP INDEX Derivations_index;
DROP INDEX Builds_Derivations_index;

ALTER TABLE Outputs RENAME TO tmp_Outputs;
ALTER TABLE Builds RENAME TO tmp_Builds;

CREATE TABLE Builds (
  derivation    TEXT NOT NULL PRIMARY KEY,
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

CREATE TABLE Outputs (
  derivation TEXT NOT NULL,
  name TEXT NOT NULL,
  path TEXT NOT NULL,
  PRIMARY KEY (derivation, name),
  FOREIGN KEY (derivation) REFERENCES Builds (derivation)
);

INSERT OR IGNORE INTO Builds (derivation, evaluation, job_name, system, nix_name, log, status, timestamp, starttime, stoptime)
SELECT Derivations.derivation, Derivations.evaluation, Derivations.job_name, Derivations.system, Derivations.nix_name,
       tmp_Builds.log, tmp_Builds.status, tmp_Builds.timestamp, tmp_Builds.starttime, tmp_Builds.stoptime
FROM Derivations
INNER JOIN tmp_Builds ON tmp_Builds.derivation = Derivations.derivation
                     AND tmp_Builds.evaluation = Derivations.evaluation;

INSERT OR IGNORE INTO Outputs (derivation, name, path)
SELECT tmp_Builds.derivation, tmp_Outputs.name, tmp_Outputs.path
FROM tmp_Outputs
INNER JOIN tmp_Builds on tmp_Builds.id = tmp_Outputs.build;

CREATE INDEX Builds_index ON Builds(job_name, system, status ASC, timestamp ASC, derivation, evaluation, stoptime DESC);

DROP TABLE tmp_Builds;
DROP TABLE tmp_Outputs;
DROP TABLE Derivations;

COMMIT;
