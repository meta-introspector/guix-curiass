BEGIN TRANSACTION;

CREATE INDEX Builds_job_name_timestamp on Builds(job_name, timestamp);

COMMIT;
