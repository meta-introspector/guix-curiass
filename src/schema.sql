BEGIN TRANSACTION;

CREATE TABLE Specifications (
  repo_name     TEXT NOT NULL PRIMARY KEY,
  url           TEXT NOT NULL,
  load_path     TEXT NOT NULL,
  file          TEXT NOT NULL,
  proc          TEXT NOT NULL,
  arguments     TEXT NOT NULL,
  -- The following columns are optional.
  branch        TEXT,
  tag           TEXT,
  revision      TEXT,
  no_compile_p  INTEGER
);

CREATE TABLE Stamps (
  specification TEXT NOT NULL PRIMARY KEY,
  stamp         TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (repo_name)
);

CREATE TABLE Evaluations (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  specification TEXT NOT NULL,
  revision      TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (repo_name)
);

CREATE TABLE Derivations (
  derivation    TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  job_name      TEXT NOT NULL,
  PRIMARY KEY (derivation, evaluation),
  FOREIGN KEY (evaluation) REFERENCES Evaluations (id)
);

-- Builds are not in a one to one relationship with derivations in order to
-- keep track of non deterministic compilations.
CREATE TABLE Builds (
  derivation    TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  log           TEXT NOT NULL,
  output        TEXT,		-- NULL if build failed
  PRIMARY KEY (derivation, evaluation, output),
  FOREIGN KEY (derivation) REFERENCES Derivations (derivation),
  FOREIGN KEY (evaluation) REFERENCES Evaluations (id)
);

COMMIT;
