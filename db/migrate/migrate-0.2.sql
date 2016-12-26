ALTER TABLE jason_project ADD COLUMN `wiki_id` BIGINT NULL;

ALTER TABLE jason_ticket ADD COLUMN `progress` INTEGER NOT NULL;

INSERT INTO entity_type (name) VALUES
("awa_job")
;
