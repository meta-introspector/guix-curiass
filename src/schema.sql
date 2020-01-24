BEGIN TRANSACTION;

CREATE TABLE Specifications (
  name          TEXT NOT NULL PRIMARY KEY,
  load_path_inputs TEXT NOT NULL, -- list of input names whose load path will be in Guile's %load-path
  package_path_inputs TEXT NOT NULL, -- list of input names whose load paths will be in GUIX_PACKAGE_PATH
  proc_input    TEXT NOT NULL, -- name of the input containing the proc that does the evaluation
  proc_file     TEXT NOT NULL, -- file containing the procedure that does the evaluation, relative to proc_input
  proc          TEXT NOT NULL, -- defined in proc_file
  proc_args     TEXT NOT NULL  -- passed to proc
);

CREATE TABLE Inputs (
  specification TEXT NOT NULL,
  name          TEXT NOT NULL,
  url           TEXT NOT NULL,
  load_path     TEXT NOT NULL,
  -- The following columns are optional.
  branch        TEXT,
  tag           TEXT,
  revision      TEXT,
  no_compile_p  INTEGER,
  PRIMARY KEY (specification, name),
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

CREATE TABLE Evaluations (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  specification TEXT NOT NULL,
  in_progress   INTEGER NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (name)
);

CREATE TABLE Outputs (
  derivation TEXT NOT NULL,
  name TEXT NOT NULL,
  path TEXT NOT NULL PRIMARY KEY,
  FOREIGN KEY (derivation) REFERENCES Builds (derivation)
);

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

CREATE TABLE Events (
  id            INTEGER PRIMARY KEY,
  type          TEXT NOT NULL,
  timestamp     INTEGER NOT NULL,
  event_json    TEXT NOT NULL
);

-- Create indexes to speed up common queries, in particular those
-- corresponding to /api/latestbuilds and /api/queue HTTP requests.
CREATE INDEX Builds_index ON Builds(job_name, system, status ASC, timestamp ASC, derivation, evaluation, stoptime DESC);
CREATE INDEX Inputs_index ON Inputs(specification, name, branch);

COMMIT;
