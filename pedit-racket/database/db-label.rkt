#lang racket

(require db)

(provide db-label-get db-label-add db-label-rename db-label-set-node db-label-del)

(define (db-label-get db node-id)
  (query-rows db "select LabelID, Name from Label where TextNodeID=$1;" node-id))

(define (db-label-add db node-id name)
  (query-exec db "insert into Label (TextNodeID, Name) values($1, $2);" node-id name))

(define (db-label-rename db label-id name)
  (query-exec db "update Label set Name=$1 where LabelID=$2;" name label-id))

(define (db-label-set-node db label-id node-id)
  (query-exec db "update Label set TextNodeID=$1 where LabelID=$2;" node-id label-id))

(define (db-label-del db id)
  (query-exec db "delete from Label where LabelID=($1);" id))
