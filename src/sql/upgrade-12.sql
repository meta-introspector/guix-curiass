BEGIN TRANSACTION;

DROP TRIGGER build_dependencies ON Builds;
DROP FUNCTION update_build_dependencies;

COMMIT;
