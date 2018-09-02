BEGIN TRANSACTION;

ALTER TABLE Outputs RENAME TO tmp_Outputs;

CREATE TABLE Outputs (
  derivation TEXT NOT NULL,
  name TEXT NOT NULL,
  path TEXT NOT NULL PRIMARY KEY,
  FOREIGN KEY (derivation) REFERENCES Builds (derivation)
);

INSERT OR IGNORE INTO Outputs (derivation, name, path)
SELECT derivation, name, path
FROM tmp_Outputs;

DROP TABLE tmp_Outputs;

COMMIT;
