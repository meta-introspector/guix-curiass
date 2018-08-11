BEGIN TRANSACTION;

ALTER TABLE Evaluations RENAME TO tmp_Evaluations;

CREATE TABLE Evaluations (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  specification TEXT NOT NULL,
  in_progress   INTEGER NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (name)
);

CREATE TABLE Checkouts (
  specification TEXT NOT NULL,
  revision      TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  input         TEXT NOT NULL,
  directory     TEXT NOT NULL,
  PRIMARY KEY (specification, revision),
  FOREIGN KEY (evaluation) REFERENCES Evaluations (id),
  FOREIGN KEY (specification) REFERENCES Specifications (name),
  FOREIGN KEY (input) REFERENCES Inputs (name)
);

INSERT INTO Evaluations (id, specification, in_progress)
SELECT id, specification, false
FROM tmp_Evaluations;

-- Copied from https://www.samuelbosch.com/2018/02/split-into-rows-sqlite.html.
INSERT OR IGNORE INTO Checkouts (specification, revision, evaluation, input, directory)
WITH RECURSIVE split(id, specification, revision, rest) AS (
  SELECT id, specification, '', commits || ' ' FROM tmp_Evaluations
   UNION ALL
  SELECT id,
         specification,
         substr(rest, 0, instr(rest, ' ')),
         substr(rest, instr(rest, ' ') + 1)
    FROM split
   WHERE rest <> '')
SELECT specification, revision, id, 'unknown', 'unknown'
  FROM split
 WHERE revision <> '';

DROP TABLE tmp_Evaluations;
DROP TABLE Stamps;

COMMIT;
