/* File generated automatically by dynamo */
/*  */
CREATE TABLE jason_attribute_definition (
  /* the attribute identifier. */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the attribute name or label. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the default value. */
  `default_value` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `project_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The project describes the base information for the project management. */
CREATE TABLE jason_project (
  /* the project identifier */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version */
  `version` INTEGER NOT NULL,
  /* the project name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the project creation date. */
  `create_date` DATETIME NOT NULL,
  /* the project status. */
  `status` TINYINT NOT NULL,
  /* the last ticket number that was allocated. */
  `last_ticket` INTEGER NOT NULL,
  /*  */
  `update_date` DATETIME NOT NULL,
  /* the project description. */
  `description` text NOT NULL,
  /*  */
  `wiki_id` BIGINT ,
  /* the project owner. */
  `owner_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE jason_attribute (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `value` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the attribute definition. */
  `definition_id` BIGINT NOT NULL,
  /*  */
  `ticket_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE jason_ticket (
  /* the ticket identifier. */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the ticket summary. */
  `summary` VARCHAR(255) BINARY NOT NULL,
  /* the ticket project unique identifier. */
  `ident` INTEGER NOT NULL,
  /* the ticket creation date. */
  `create_date` DATETIME NOT NULL,
  /* the ticket priority. */
  `priority` INTEGER NOT NULL,
  /* the ticket status. */
  `status` TInYINT NOT NULL,
  /* the ticket description. */
  `description` TEXT NOT NULL,
  /* the last ticket update date. */
  `update_date` DATETIME NOT NULL,
  /* the ticket type. */
  `ticket_type` TINYINT NOT NULL,
  /* the duration to resolve the ticket. */
  `duration` INTEGER NOT NULL,
  /* the progress percentation (0 .. 100). */
  `progress` INTEGER NOT NULL,
  /*  */
  `project_id` BIGINT NOT NULL,
  /*  */
  `creator_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT INTO entity_type (name) VALUES
("jason_attribute_definition")
,("jason_project")
,("jason_attribute")
,("jason_ticket")
;
