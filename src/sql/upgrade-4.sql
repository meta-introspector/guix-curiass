BEGIN TRANSACTION;

ALTER TABLE Builds ADD COLUMN weather INTEGER;
CREATE INDEX Builds_weather_evaluation ON Builds (weather, evaluation);

COMMIT;
