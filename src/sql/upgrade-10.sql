BEGIN TRANSACTION;

-- Make sure that the cached Job build status is always synchronized with the
-- matching build status.
CREATE FUNCTION update_job_status()
RETURNS TRIGGER AS $$
BEGIN
UPDATE Jobs SET status = NEW.status WHERE Jobs.build = NEW.id;
RETURN null;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER build_status AFTER UPDATE ON Builds
FOR EACH ROW
EXECUTE PROCEDURE update_job_status();

COMMIT;
