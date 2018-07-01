ALTER TABLE person ADD COLUMN nickname label NOT NULL DEFAULT '("","","","")';
ALTER TABLE person ALTER COLUMN nickname DROP DEFAULT;

ALTER TABLE person ADD COLUMN display_order int NOT NULL DEFAULT 0;
UPDATE person SET display_order = o.n_ FROM (SELECT id AS id_, row_number() OVER (ORDER BY id) AS n_ FROM person) AS o WHERE id = o.id_;
ALTER TABLE person ADD UNIQUE (display_order);
ALTER TABLE person ALTER COLUMN display_order DROP DEFAULT;