BEGIN TRANSACTION;

CREATE INDEX Builds_status_index ON Builds (status);

CREATE INDEX Outputs_derivation_index ON Outputs (derivation);

COMMIT;
