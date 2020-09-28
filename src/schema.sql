BEGIN TRANSACTION;

CREATE TABLE Specifications (
  name          TEXT NOT NULL PRIMARY KEY,
  load_path_inputs TEXT NOT NULL, -- list of input names whose load path will be in Guile's %load-path
  package_path_inputs TEXT NOT NULL, -- list of input names whose load paths will be in GUIX_PACKAGE_PATH
  proc_input    TEXT NOT NULL, -- name of the input containing the proc that does the evaluation
  proc_file     TEXT NOT NULL, -- file containing the procedure that does the evaluation, relative to proc_input
  proc          TEXT NOT NULL, -- defined in proc_file
  proc_args     TEXT NOT NULL,  -- passed to proc
  build_outputs TEXT NOT NULL --specify what build outputs should be made available for download
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
  timestamp     INTEGER NOT NULL,
  PRIMARY KEY (specification, revision),
  FOREIGN KEY (evaluation) REFERENCES Evaluations (id),
  FOREIGN KEY (specification) REFERENCES Specifications (name),
  FOREIGN KEY (input) REFERENCES Inputs (name)
);

CREATE TABLE Evaluations (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  specification TEXT NOT NULL,
  status        INTEGER NOT NULL,
  timestamp     INTEGER NOT NULL,
  checkouttime  INTEGER NOT NULL,
  evaltime      INTEGER NOT NULL,
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

CREATE TABLE Metrics (
  field         INTEGER NOT NULL,
  type          INTEGER NOT NULL,
  value         DOUBLE PRECISION NOT NULL,
  timestamp     INTEGER NOT NULL,
  PRIMARY KEY (field, type)
);

CREATE TABLE BuildProducts (
  build         INTEGER NOT NULL,
  type          TEXT NOT NULL,
  file_size     BIGINT NOT NULL,
  checksum      TEXT NOT NULL,
  path          TEXT NOT NULL,
  PRIMARY KEY (build, path)
  FOREIGN KEY (build) REFERENCES Builds (id) ON DELETE CASCADE
);

CREATE TABLE Events (
  id            INTEGER PRIMARY KEY,
  type          TEXT NOT NULL,
  timestamp     INTEGER NOT NULL,
  event_json    TEXT NOT NULL
);

-- Create indexes to speed up common queries.
CREATE INDEX Builds_status_index ON Builds (status);
CREATE INDEX Builds_evaluation_index ON Builds (evaluation, status);
CREATE INDEX Evaluations_status_index ON Evaluations (id, status);
CREATE INDEX Evaluations_specification_index ON Evaluations (specification, id DESC);
CREATE INDEX Outputs_derivation_index ON Outputs (derivation);
CREATE INDEX Inputs_index ON Inputs(specification, name, branch);

COMMIT;
