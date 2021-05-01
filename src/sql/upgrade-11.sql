BEGIN TRANSACTION;

CREATE FUNCTION build_dependencies(build bigint)
RETURNS TABLE (dependencies text) AS $$
SELECT string_agg(cast(BD.target AS text), ',')
FROM BuildDependencies as BD
WHERE BD.source = $1
$$ LANGUAGE sql;

CREATE VIEW pending_dependencies AS
SELECT Builds.id, count(dep.id) as deps FROM Builds
LEFT JOIN BuildDependencies as bd ON bd.source = Builds.id
LEFT JOIN Builds AS dep ON bd.target = dep.id AND dep.status != 0
WHERE Builds.status = -2 GROUP BY Builds.id;

CREATE OR REPLACE FUNCTION update_build_dependencies()
RETURNS TRIGGER AS $$
BEGIN
IF NEW.status > 0 AND NEW.status != OLD.status THEN

WITH RECURSIVE deps AS (
SELECT source FROM BuildDependencies WHERE target = NEW.id
UNION
SELECT BD.source FROM deps INNER JOIN BuildDependencies as BD
ON BD.target = deps.source)

UPDATE Builds SET status =
CASE
WHEN NEW.status = 4 THEN 4 ELSE 2 END
FROM deps WHERE Builds.id = deps.source;
END IF;
RETURN null;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER build_dependencies AFTER UPDATE ON Builds
FOR EACH ROW
WHEN (pg_trigger_depth() = 0)
EXECUTE PROCEDURE update_build_dependencies();

CREATE INDEX BuildDependencies_target ON BuildDependencies(target); --speeds up delete cascade.

COMMIT;
