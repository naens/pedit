#lang racket

(require db)

(provide db-language-get-all db-language-add db-language-rename db-language-del)

(define (db-language-get-all db)
   (query-rows db "select LanguageID, Name from Language;"))

(define (db-language-add db name)
  (query-exec db "insert into Language(Name) values($1);" name))

(define (db-language-rename db id new-name)
  (query-exec db "update Language set Name=$1 where LanguageID=$2" new-name id))

(define (db-language-del db id)
  (query-exec db "delete from Language where LanguageID=($1);" id))
