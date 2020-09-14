BEGIN TRANSACTION;

CREATE TABLE Metrics (
  field         INTEGER NOT NULL,
  type          INTEGER NOT NULL,
  value         DOUBLE PRECISION NOT NULL,
  timestamp     INTEGER NOT NULL,
  PRIMARY KEY (field, type)
);

COMMIT;
