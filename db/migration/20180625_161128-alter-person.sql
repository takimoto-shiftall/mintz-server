ALTER TABLE person ADD COLUMN nickname label NOT NULL DEFAULT '("","","","")';
ALTER TABLE person ALTER COLUMN nickname DROP DEFAULT;