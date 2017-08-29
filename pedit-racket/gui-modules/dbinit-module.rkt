#lang racket/gui

(require db)

(require "text-module.rkt")

(define nil '())
(define init-script-fn "/home/andrei/projects/pedit/create_db.sql")

; Make a frame by instantiating the frame% class
(define frame (new frame% (label "Pedit - Database Init Module")))
 
(new message% (parent frame)
     (label "Pedit: Database Init Module"))

;; Text Panel
(define db-panel (new horizontal-panel% (parent frame)))

(define db-text-field (new text-field% (parent db-panel)
                         (min-width 300)
                         (label "Database File")
                         (init-value "tmp.db")))


(new button% (parent db-panel)
             (label "Get-File")
             (callback (lambda (button event)
                         (let ((fn (get-file)))
                           (when fn
                               (send db-text-field set-value (path->string fn)))))))

(define (connect-db db-path)
  (let ((db (sqlite3-connect #:database db-path)))
    (show-text-module db)))

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
    (show-text-module db)))

(new button% (parent frame)
             (label "Open/Create")
             (callback (lambda (button event)
                         (let ((fp (string->path (send db-text-field get-value))))
                           (if (file-exists? fp)
                               (connect-db fp)
                               (create-db fp))))))

;; Exit Button
(new button% (parent frame)
             (label "Exit")
             (callback (lambda (button event)
                         (send frame show #f))))

; Show the frame by calling its show method
(send frame show #t)
