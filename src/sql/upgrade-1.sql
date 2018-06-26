BEGIN TRANSACTION;

DROP INDEX Specifications_index;

ALTER TABLE Specifications RENAME TO tmp_Specifications;
ALTER TABLE Stamps RENAME TO tmp_Stamps;
ALTER TABLE Evaluations RENAME TO tmp_Evaluations;

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

CREATE TABLE Stamps (
  specification TEXT NOT NULL PRIMARY KEY,
  stamp         TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (name)
);

CREATE TABLE Evaluations (
  id            INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  specification TEXT NOT NULL,
  commits       TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications (name)
);

INSERT INTO Specifications (name, load_path_inputs, package_path_inputs, proc_input, proc_file, proc, proc_args)
SELECT printf('%s-%s', repo_name, branch) AS name,
       printf('("%s")', repo_name)        AS load_path_inputs,
       '()'                               AS package_path_inputs,
       repo_name                          AS proc_input,
       file                               AS proc_file,
       proc,
       arguments                          AS proc_args
FROM tmp_Specifications;

INSERT INTO Inputs (specification, name, url, load_path, branch, tag, revision, no_compile_p)
SELECT printf('%s-%s', repo_name, branch) AS specification,
       repo_name                          AS name,
       url, load_path, branch, tag, revision, no_compile_p
FROM tmp_Specifications;

INSERT INTO Stamps (specification, stamp)
SELECT Specifications.name AS specification, stamp
FROM tmp_Stamps
LEFT JOIN Specifications ON Specifications.proc_input = tmp_Stamps.specification;

INSERT INTO Evaluations (id, specification, commits)
SELECT id, Specifications.name AS specification, revision
FROM tmp_Evaluations
LEFT JOIN Specifications ON Specifications.proc_input = tmp_Evaluations.specification;

CREATE INDEX Inputs_index ON Inputs(specification, name, branch);

DROP TABLE tmp_Specifications;
DROP TABLE tmp_Stamps;
DROP TABLE tmp_Evaluations;

COMMIT;
