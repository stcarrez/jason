UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_workspace'
  SET awa_acl.workspace_id = awa_acl.entity_id;

UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_wiki_space'
  INNER JOIN awa_wiki_space ON awa_acl.entity_id = awa_wiki_space.id
  SET awa_acl.workspace_id = awa_wiki_space.workspace_id;

UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_blog'
  INNER JOIN awa_blog ON awa_acl.entity_id = awa_blog.id
  SET awa_acl.workspace_id = awa_blog.workspace_id;

INSERT INTO awa_permission (id, name) VALUES
 (1, 'blog-create'),
 (2, 'blog-delete'),
 (3, 'blog-create-post'),
 (4, 'blog-delete-post'),
 (5, 'blog-add-comment'),
 (6, 'blog-publish-comment'),
 (7, 'blog-delete-comment'),
 (8, 'blog-update-post'),
 (9, 'workspace-create'),
 (10, 'wiki-page-create'),
 (11, 'wiki-page-delete'),
 (12, 'wiki-page-update'),
 (13, 'wiki-page-view'),
 (14, 'wiki-space-create'),
 (15, 'wiki-space-delete'),
 (16, 'wiki-space-update'),
 (17, 'storage-create'),
 (18, 'storage-delete'),
 (19, 'folder-create');
INSERT INTO awa_permission (id, name) VALUES
 (20, 'project-create'),
 (21, 'project-update'),
 (22, 'project-delete'),
 (23, 'ticket-create'),
 (24, 'ticket-delete'),
 (25, 'ticket-update')
;

CREATE TEMPORARY TABLE new_acl (
  id integer auto_increment primary key,
  entity_id BIGINT NOT NULL,
  user_id BIGINT NOT NULL,
  workspace_id BIGINT NOT NULL,
  entity_type INTEGER NOT NULL,
  permission INTEGER NOT NULL
);

/* Permissions on the workspace.  */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
      IN ('blog-create', 'wiki-space-create', 'storage-create', 'folder-create', 'storage-delete',
          'project-create')
  WHERE e.name = 'awa_workspace';

/* Permissions on the blog entries */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
    IN ('blog-update-post', 'blog-delete', 'blog-create-post', 'blog-delete-post',
        'blog-add-comment', 'blog-publish-comment', 'blog-delete-comment')
  WHERE e.name = 'awa_blog';

/* Permissions on the wiki entries */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
    IN ('wiki-space-update', 'wiki-space-delete', 'wiki-page-create', 'wiki-page-delete',
        'wiki-page-update', 'wiki-page-view')
  WHERE e.name = 'awa_wiki_space';

/* Permissions to create tickets on projects */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
    IN ('project-update', 'project-delete', 'ticket-create',
        'ticket-update', 'ticket-delete')
  WHERE e.name = 'jason_project';

/* Install the new permissions.  */
DELETE FROM awa_acl;
INSERT INTO awa_acl (id, entity_id, user_id, workspace_id, entity_type, permission)
  SELECT id, entity_id, user_id, workspace_id, entity_type, permission FROM new_acl;

UPDATE sequence
  SET value = GREATEST((SELECT COUNT(*) FROM new_acl), sequence.value)
  WHERE sequence.name = 'awa_acl';

DROP TEMPORARY TABLE new_acl;

