#lang racket

(require db)

(require "db-lang.rkt")
(require "db-text.rkt")
(require "db-tv.rkt")

(provide connect-db create-db)

;; from language module
(provide db-lang-get-all db-lang-add db-lang-rename db-lang-del)

;; from text module
(provide db-text-get-by-lang db-text-add db-text-rename db-text-set-lang
         db-text-del)

;; from text version module
(provide db-tv-get-by-text db-tv-add db-tv-rename db-tv-del)

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