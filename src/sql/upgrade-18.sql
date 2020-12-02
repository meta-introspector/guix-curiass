BEGIN TRANSACTION;

CREATE TABLE Workers (
  name        TEXT NOT NULL PRIMARY KEY,
  address     TEXT NOT NULL,
  systems     TEXT NOT NULL,
  last_seen   INTEGER NOT NULL
);

COMMIT;
