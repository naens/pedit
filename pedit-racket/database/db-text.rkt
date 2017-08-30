#lang racket

(require db)

(provide db-text-get-by-language db-text-add db-text-rename db-text-set-language db-text-del db-text-del-by-language)

(define (db-text-get-by-language db language-id)
  (query-rows db "select TextId, Name from Text where LanguageID=$1;" language-id))

(define (db-text-add db language-id name)
  (query-exec db "insert into Text(LanguageID, Name) values($1, $2);" language-id name))

(define (db-text-rename db id new-name)
  (query-exec db "update Text set Name=$1 where TextID=$2" new-name id))

(define (db-text-set-language db id language-id)
  (query-exec db "update Text set LanguageID=$1 where TextID=$2" language-id id))

(define (db-text-del db id)
  (query-exec db "delete from Text where TextID=($1);" id))

(define (db-text-del-by-language db language-id)
  (query-exec db "delete from Text where LanguageID=($1);" language-id))
