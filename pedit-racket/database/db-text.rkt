#lang racket

(require db)

(provide db-text-get-by-lang db-text-add db-text-rename db-text-set-lang db-text-del)

(define (db-text-get-by-lang db lang-id)
  (query-rows db "select TextId, Name from Text where LanguageID=$1;" lang-id))

(define (db-text-add db lang-id name)
  (query-exec db "insert into Text(LanguageID, Name) values($1, $2);" lang-id name))

(define (db-text-rename db id new-name)
  (query-exec db "update Text set Name=$1 where TextID=$2" new-name id))

(define (db-text-set-lang db id lang-id)
  (query-exec db "update Text set LanguageID=$1 where TextID=$2" lang-id id))

(define (db-text-del db id)
  (query-exec db "delete from Text where TextID=($1);" id))
