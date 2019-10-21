BEGIN TRANSACTION;

CREATE TABLE Events (
  id            INTEGER PRIMARY KEY,
  type          TEXT NOT NULL,
  timestamp     INTEGER NOT NULL,
  event_json    TEXT NOT NULL
);

CREATE TABLE EventsOutbox (
  event_id INTEGER NOT NULL,
  FOREIGN KEY (event_id) REFERENCES Events (id)
);

COMMIT;
