#lang racket

(require db)
(require racket/runtime-path)

(require "db-lang.rkt")
(require "db-text.rkt")
(require "db-tv.rkt")
(require "db-label.rkt")
(require "db-node.rkt")
(require "db-text-cell.rkt")
(require "db-permutation.rkt")

(provide connect-db create-db)

;; from language module
(provide db-lang-get-all db-lang-add db-lang-rename db-lang-del)

;; from text module
(provide db-text-get-by-lang db-text-add db-text-rename db-text-set-lang
         db-text-del)

;; from text version module
(provide (struct-out tv) db-tv-get db-tv-get-by-text db-tv-list-by-text db-tv-add
         db-tv-rename
         db-tv-get-seps
         db-tv-set-pre-chrs db-tv-set-post-chrs db-tv-set-sep-chrs
         db-tv-del)

;; from label module
(provide db-label-get db-label-add db-label-rename db-label-set-node db-label-del)

;; from node module
(provide db-node-get-first db-node-get-by-id db-node-get-list db-node-get-before db-node-get-after
         db-node-add-first db-node-insert-before db-node-add-last
         db-node-move-first db-node-move-before db-node-move-last
         db-node-del)

;; from permutation module
(provide db-permutation-src db-permutation-dest db-permutation-set db-permutation-del)

;; from text cell module
(provide db-text-cell-get db-text-cell-get-text db-text-cell-get-pre db-text-cell-get-post db-text-cell-set)

;; common definitions
;;(define init-script-fn "../../create_db.sql")
(define-runtime-path db-init-file "../../create_db.sql")


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
        ;;(init-script (file->string (string->path init-script-fn)))
        (init-script (file->string db-init-file))
        )
    (run-script db (string-split init-script ";"))
    db))

