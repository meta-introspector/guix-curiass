BEGIN TRANSACTION;

CREATE INDEX Builds_id_job_status_index ON Builds (id DESC, job_name, status);

COMMIT;
