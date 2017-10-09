#lang racket

(require db)

(provide db-text-cell-get db-text-cell-set)

(define (db-text-cell-get db tv-id node-id)
  (let ((row (query-maybe-row db "select Pre, Post from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (when row
      (let ((text (query-list db "select WordPart.Text from WordPart natural join TextItem natural join TextItemTextVersion where TextItem.TextNodeID=0 and TextItemTextVersion.TextItemID=1 order by WordPart.TI_O;")))
        (values (vector-ref row 0) (apply string-append text) (vector-ref row 1))))))

(define (db-text-cell-set db tv-id node-id pre text post)
  ;; set pre/post to cell
  (let ((row (query-maybe-row db "select Pre, Post from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (if row
        (query-exec db "update TextCell set Pre=$1, Post=$2 where TextVersionID=$1 and TextNodeID=$2;" pre post tv-id node-id)
        (query-exec db "insert into TextCell (TextVersionID, TextNodeID, Pre, Post) values ($1, $2, $3, $4);" tv-id node-id pre post)))

  ;; get text-item
  (let ((rows (query-rows db "select TextItemID from TextItem where TextNodeID=$1;")))
    '<BODY>
    ;; if exists a ti with tv connection: remove connection
    (query-exec db "delete from TextItemTextVersion where TextVersionID=$1 and TextItemID in (select TextItemID from TextItem where TextNodeID=$2);" tv-id node-id)
    (query-exec db "delete from TextItem where TextNodeID=$1 and (select count(*) from TextItemTextVersion where TextItem.TextItemID = TextItemTextVersion.TextItemID) = 0;" node-id tv-id) 

    ;; todo: for each row: generate sum wp string, find first where equal to string

    ;; todo: if found: connect tv to this text item

    ;; todo: if not found: create new and connect
))