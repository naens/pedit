#lang racket

(require db)

(provide db-language-get-all)

(define (db-language-get-all db)
   (query-rows db "select LanguageID, Name from Language"))
