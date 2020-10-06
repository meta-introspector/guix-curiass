BEGIN TRANSACTION;

CREATE INDEX Builds_timestamp_stoptime on Builds(timestamp, stoptime);

COMMIT;
