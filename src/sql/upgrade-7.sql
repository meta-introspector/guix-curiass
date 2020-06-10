BEGIN TRANSACTION;

CREATE TABLE BuildProducts (
  build         INTEGER NOT NULL,
  type          TEXT NOT NULL,
  file_size     BIGINT NOT NULL,
  checksum      TEXT NOT NULL,
  path          TEXT NOT NULL,
  PRIMARY KEY (build, path)
  FOREIGN KEY (build) REFERENCES Builds (id) ON DELETE CASCADE
);

ALTER TABLE Specifications ADD build_outputs TEXT NOT NULL DEFAULT "()";

COMMIT;
