BEGIN TRANSACTION;

CREATE TABLE Dashboards (
  id              TEXT NOT NULL PRIMARY KEY,
  specification   TEXT NOT NULL,
  jobs            TEXT NOT NULL,
  FOREIGN KEY (specification) REFERENCES Specifications(name) ON DELETE CASCADE
);

COMMIT;
