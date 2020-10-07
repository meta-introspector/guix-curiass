BEGIN TRANSACTION;

CREATE INDEX Builds_stoptime on Builds(stoptime DESC);
CREATE INDEX Builds_stoptime_id on Builds(stoptime DESC, id DESC);
CREATE INDEX Builds_status_ts_id on Builds(status DESC, timestamp DESC, id ASC);

COMMIT;
