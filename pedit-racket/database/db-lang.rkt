#lang racket

(require db)

(provide db-lang-get-all db-lang-add db-lang-rename db-lang-del)

(define (db-lang-get-all db)
   (query-rows db "select LanguageID, Name from Language;"))

(define (db-lang-add db name)
  (query-exec db "insert into Language(Name) values($1);" name))

(define (db-lang-rename db id new-name)
  (query-exec db "update Language set Name=$1 where LanguageID=$2" new-name id))

(define (db-lang-del db id)
  (query-exec db "delete from Language where LanguageID=($1);" id))
