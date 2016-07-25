BEGIN TRANSACTION;

CREATE TABLE Specifications (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  repo_name     TEXT NOT NULL,
  url           TEXT NOT NULL,
  load_path     TEXT NOT NULL,
  file          TEXT NOT NULL,
  proc          TEXT NOT NULL,
  arguments     TEXT NOT NULL,
  -- The following columns are optional.
  branch        TEXT,
  tag           TEXT,
  revision      TEXT
);

CREATE TABLE Stamps (
  specification INTEGER NOT NULL PRIMARY KEY,
  stamp         TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (id)
);

CREATE TABLE Evaluations (
  derivation    TEXT NOT NULL PRIMARY KEY,
  job_name      TEXT NOT NULL,
  specification INTEGER NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (id)
);

CREATE TABLE Builds (
  id              INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  derivation      TEXT NOT NULL,
  log             TEXT NOT NULL,
  output          TEXT,         -- NULL if build failed
  FOREIGN KEY (derivation) REFERENCES Evaluations (derivation)
);

COMMIT;
