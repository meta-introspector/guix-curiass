BEGIN TRANSACTION;

CREATE TABLE SchemaVersion (
    version     INTEGER NOT NULL
);

CREATE TABLE Specifications (
  name          TEXT NOT NULL PRIMARY KEY,
  build         TEXT NOT NULL,
  channels      TEXT NOT NULL,
  build_outputs TEXT NOT NULL,
  notifications TEXT NOT NULL,
  period        INTEGER NOT NULL DEFAULT 0,
  priority      INTEGER NOT NULL DEFAULT 0,
  systems       TEXT NOT NULL,
  is_active     INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE Evaluations (
  id            SERIAL PRIMARY KEY,
  specification TEXT NOT NULL,
  status        INTEGER NOT NULL,
  timestamp     INTEGER NOT NULL,
  checkouttime  INTEGER NOT NULL,
  evaltime      INTEGER NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications(name) ON DELETE CASCADE
);

CREATE TABLE Checkouts (
  specification TEXT NOT NULL,
  revision      TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  channel       TEXT NOT NULL,
  directory     TEXT NOT NULL,
  timestamp     INTEGER NOT NULL,
  PRIMARY KEY (specification, revision),
  FOREIGN KEY (evaluation) REFERENCES Evaluations(id) ON DELETE CASCADE,
  FOREIGN KEY (specification) REFERENCES Specifications(name) ON DELETE CASCADE
);

CREATE TABLE Builds (
  id            SERIAL PRIMARY KEY,
  derivation    TEXT NOT NULL UNIQUE,
  evaluation    INTEGER NOT NULL,
  job_name      TEXT NOT NULL,
  system        TEXT NOT NULL,
  worker        TEXT, --optional, worker performing the build.
  nix_name      TEXT NOT NULL,
  log           TEXT NOT NULL,
  status        INTEGER NOT NULL,
  last_status   INTEGER,
  weather       INTEGER,
  priority      INTEGER NOT NULL DEFAULT 0,
  max_silent    INTEGER NOT NULL DEFAULT 0,
  timeout       INTEGER NOT NULL DEFAULT 0,
  timestamp     INTEGER NOT NULL,
  starttime     INTEGER NOT NULL,
  stoptime      INTEGER NOT NULL,
  FOREIGN KEY (evaluation) REFERENCES Evaluations(id) ON DELETE CASCADE
);

CREATE TABLE BuildDependencies (
  source        INTEGER NOT NULL,
  target        INTEGER NOT NULL,
  PRIMARY KEY (source, target),
  FOREIGN KEY (source) REFERENCES Builds(id) ON DELETE CASCADE,
  FOREIGN KEY (target) REFERENCES Builds(id) ON DELETE CASCADE
);

CREATE TABLE Jobs (
  name          TEXT NOT NULL,
  evaluation    INTEGER NOT NULL,
  build         INTEGER NOT NULL,
  status        INTEGER NOT NULL, --caches Builds.status
  system        TEXT NOT NULL, --caches Builds.system
  PRIMARY KEY (evaluation, build),
  FOREIGN KEY (build) REFERENCES Builds(id) ON DELETE CASCADE,
  FOREIGN KEY (evaluation) REFERENCES Evaluations(id) ON DELETE CASCADE
);

CREATE TABLE Outputs (
  derivation TEXT NOT NULL,
  name TEXT NOT NULL,
  path TEXT NOT NULL PRIMARY KEY,
  FOREIGN KEY (derivation) REFERENCES Builds(derivation) ON DELETE CASCADE
);

CREATE TABLE Metrics (
  id            SERIAL,
  field         TEXT NOT NULL,
  type          INTEGER NOT NULL,
  value         DOUBLE PRECISION NOT NULL,
  timestamp     INTEGER NOT NULL,
  PRIMARY KEY (field, type)
);

CREATE TABLE BuildProducts (
  id            SERIAL,
  build         INTEGER NOT NULL,
  type          TEXT NOT NULL,
  file_size     BIGINT NOT NULL,
  checksum      TEXT NOT NULL,
  path          TEXT NOT NULL,
  PRIMARY KEY (build, path),
  FOREIGN KEY (build) REFERENCES Builds(id) ON DELETE CASCADE
);

CREATE TABLE Notifications (
  id            SERIAL PRIMARY KEY,
  type          TEXT NOT NULL,
  build         INTEGER NOT NULL,
  FOREIGN KEY (build) REFERENCES Builds(id) ON DELETE CASCADE
);

CREATE TABLE Workers (
  name        TEXT NOT NULL PRIMARY KEY,
  address     TEXT NOT NULL,
  machine     TEXT NOT NULL,
  systems     TEXT NOT NULL,
  last_seen   INTEGER NOT NULL
);

CREATE TABLE Dashboards (
  id              TEXT NOT NULL PRIMARY KEY,
  specification   TEXT NOT NULL,
  jobs            TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications(name) ON DELETE CASCADE
);

-- Make sure that the cached Job build status is always synchronized with the
-- matching build status.
CREATE FUNCTION update_job_status()
RETURNS TRIGGER AS $$
BEGIN
UPDATE Jobs SET status = NEW.status WHERE Jobs.build = NEW.id;
RETURN null;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER build_status AFTER UPDATE ON Builds
FOR EACH ROW
EXECUTE PROCEDURE update_job_status();

-- Return the list of comma separated dependencies of BUILD.
CREATE FUNCTION build_dependencies(build bigint)
RETURNS TABLE (dependencies text) AS $$
SELECT string_agg(cast(BD.target AS text), ',')
FROM BuildDependencies as BD
WHERE BD.source = $1
$$ LANGUAGE sql;

-- XXX: All queries targeting Builds and Outputs tables *must* be covered by
-- an index.  It is also preferable for the other tables.
CREATE INDEX Builds_status_index ON Builds (status);
CREATE INDEX Builds_evaluation_index ON Builds (evaluation, status);
CREATE INDEX Builds_job_name_timestamp on Builds(job_name, timestamp);
CREATE INDEX Builds_nix_name ON Builds (nix_name);
CREATE INDEX Builds_timestamp_stoptime on Builds(timestamp, stoptime);
CREATE INDEX Builds_stoptime on Builds(stoptime DESC);
CREATE INDEX Builds_stoptime_id on Builds(stoptime DESC, id DESC);
CREATE INDEX Builds_status_ts_id on Builds(status DESC, timestamp DESC, id ASC);
CREATE INDEX Builds_priority_timestamp on Builds(priority ASC, timestamp DESC);
CREATE INDEX Builds_weather_evaluation ON Builds (weather, evaluation);
CREATE INDEX Builds_id_job_status_index ON Builds (id DESC, job_name, status);
CREATE INDEX Jobs_name ON Jobs (name);
CREATE INDEX Jobs_system_status ON Jobs (system, status);
CREATE INDEX Jobs_build ON Jobs (build); --speeds up delete cascade.
CREATE INDEX Evaluations_status_index ON Evaluations (id, status);
CREATE INDEX Evaluations_specification_index ON Evaluations (specification, id DESC);
CREATE INDEX Outputs_derivation_index ON Outputs (derivation);
CREATE INDEX BuildProducts_build ON BuildProducts(build); --speeds up delete cascade.
CREATE INDEX Notifications_build ON Notifications(build); --speeds up delete cascade.
CREATE INDEX BuildDependencies_target ON BuildDependencies(target); --speeds up delete cascade.

COMMIT;
