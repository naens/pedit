#lang racket

(require db)
(require "db-common.rkt")

(provide db-text-cell-get db-text-cell-set)

(define (db-text-cell-get db tv-id node-id)
  (let ((row (query-maybe-row db "select Pre, Post from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (when row
      (let ((text (query-list db "select WordPart.Text from WordPart natural join TextItem natural join TextItemTextVersion where TextItem.TextNodeID=0 and TextItemTextVersion.TextItemID=1 order by WordPart.TI_O;")))
        (values (vector-ref row 0) (apply string-append text) (vector-ref row 1))))))
 
(define (db-text-cell-set db tv-id node-id pre text post)
  ;; 1. if there is already a text-cell for given tv-id/node-id pair: create new text-cell, otherwise: update existing
  (let ((row (query-maybe-row db "select * from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (if row
        (query-exec db "update TextCell set Pre=$1, Post=$2 where TextVersionID=$1 and TextNodeID=$2;" pre post tv-id node-id)
        (query-exec db "insert into TextCell (TextVersionID, TextNodeID, Pre, Post) values ($1, $2, $3, $4);" tv-id node-id pre post)))

  ;; 2. if there is a wp for given tv-id/node-id: delete it (+ also words and word-parts associated with it)
  (let ((words (query-list db "select WordID from WordPart where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (map (lambda (word-id)
           (query-exec db "delete from Word where WordID=$1;" word-id))
         words))

  ;; 3. create new wp with a single word containing text
  (db-text-cell-create-wp db tv-id node-id text)

  ;; 4. TODO: what if user wants to edit a single word part?
)

(define (db-text-cell-get-string db tv-id node-id)
  (let ((wps (query-list db "select Text from WordPart where TextVersionID=$1 and TextNodeID=$2 order by TI_O;" tv-id node-id)))
    (list->string wps)))

(define (db-text-cell-create-wp db tv-id node-id text)
  (define word-id (db-last-id db (query db "insert into Word values (NULL);")))
  (query-exec db "insert into WordPart (TextVersionID, TextNodeID, WordID, TI_O, Text) values ($1, $2, $3, $4);" tv-id node-id word-id 0 text))
