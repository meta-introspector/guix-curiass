BEGIN TRANSACTION;

CREATE INDEX Builds_evaluation_index ON Builds (evaluation, status);
CREATE INDEX Evaluations_status_index ON Evaluations (id, status);
CREATE INDEX Evaluations_specification_index ON Evaluations (specification, id DESC);

COMMIT;
