BEGIN TRANSACTION;

CREATE INDEX Jobs_build ON Jobs (build); --speeds up delete cascade.
CREATE INDEX BuildProducts_build ON BuildProducts(build); --speeds up delete cascade.
CREATE INDEX Notifications_build ON Notifications(build); --speeds up delete cascade.

COMMIT;
