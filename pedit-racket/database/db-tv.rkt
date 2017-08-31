#lang racket

(require db)
(provide db-tv-get-by-text db-tv-add db-tv-rename db-tv-del)

(define (db-tv-get-by-text db text-id)
  (query-rows db "select TextVersionID, Name from TextVersion where TextID=$1;" text-id))
  
(define (db-tv-add db text-id name)
  (query-exec db "insert into TextVersion(TextID, Name) values($1, $2);" text-id name))

(define (db-tv-rename db tv-id name)
  (query-exec db "update TextVersion set Name=$1 where TextVersionID=$2;" name tv-id))

(define (db-tv-del db tv-id)
  (query-exec db "delete from TextVersion where TextVersionID=$1;" tv-id))
