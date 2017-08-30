#lang racket

(require db)

(require "db-language.rkt")
(require "db-text.rkt")

(provide connect-db create-db)

;; from language module
(provide db-language-get-all db-language-add db-language-rename db-language-del db-text-del-by-language)

;; from text module
(provide db-text-get-by-language db-text-add db-text-rename db-text-set-language db-text-del)

(define init-script-fn "../../../create_db.sql")

(define (connect-db db-path)
  (sqlite3-connect #:database db-path))

(define (run-script db statements)
  (unless (empty? statements)
    (let ((statement (string-trim (first statements))))
      (when (non-empty-string? statement)
        (query-exec db statement))
        (run-script db (rest statements)))))


(define (create-db db-path)
  (let ((db (sqlite3-connect #:database db-path #:mode 'create))
        (init-script (file->string (string->path init-script-fn))))
    (run-script db (string-split init-script ";"))
    db))