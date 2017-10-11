#lang racket

(require db)

(require "db-common.rkt")

(provide db-text-item-get-string db-text-item-create db-connect-ti-tv)

(define (db-text-item-get-string db ti-id)
  (let ((wps (query-list db "select Text from WordPart where TextItemID=$1 order by TI_O;" ti-id)))
    (list->string wps)))

(define (db-connect-ti-tv db ti-id tv-id)
  (query-exec db "insert into TextItemTextVersion (TextItemID, TextVesionID) values ($1, $2);" ti-id tv-id))

(define (db-text-item-create db tv-id node-id text)
  (define ti-id (db-last-id db (query db "insert into TextItem (TextNodeID) values ($1);" node-id)))
  (define word-id (db-last-id db (query db "insert into Word values (NULL);")))
;;  (print (format "[ti-id:~a,word-id=~a,text=~a,tv-id=~a,node-id=~a]~%" ti-id word-id text tv-id node-id))
  (query-exec db "insert into WordPart (TextItemID, WordID, TI_O, Text) values ($1, $2, $3, $4)" ti-id word-id 0 text)
  (query-exec db "insert into TextItemTextVersion (TextItemID, TextVersionID) values ($1, $2);" ti-id tv-id))
