BEGIN TRANSACTION;

CREATE TABLE Jobs (
  name          TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  derivation    TEXT NOT NULL,
  system        TEXT NOT NULL,
  PRIMARY KEY (evaluation, derivation),
  FOREIGN KEY (evaluation) REFERENCES Evaluations(id) ON DELETE CASCADE
);

CREATE INDEX Jobs_name ON Jobs (name);

COMMIT;
