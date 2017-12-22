#lang racket/gui

(require "../database/pedit-db.rkt")

(require "text-module.rkt")

(require db)

; Make a frame by instantiating the frame% class
(define frame (new frame% (label "Pedit - Database Init Module")))

(define msg1 (new message% (parent frame)
                  (label "Pedit: Database Init Module")))

;; Text Panel
(define db-panel (new horizontal-panel% (parent frame)))

(define db-text-field (new text-field% (parent db-panel)
                         (min-width 300)
                         (label "Database File")
                         (init-value "tmp.db")))

(define btn1 (new button% (parent db-panel)
                  (label "Get-File")
                  (callback (lambda (button event)
                              (let ((fn (get-file)))
                                (when fn
                                  (send db-text-field set-value (path->string fn))))))))

(define btn2 (new button% (parent frame)
                  (label "Open/Create")
                  (callback (lambda (button event)
                              (let* ((fp (string->path (send db-text-field get-value)))
                                     (db (if (file-exists? fp)
                                             (connect-db fp)
                                             (create-db fp))))
                                (query-exec db "PRAGMA foreign_keys = ON")
                                (show-text-module db))))))

;; Exit Button
(define btn3 (new button% (parent frame)
                  (label "Exit")
                  (callback (lambda (button event)
                              (send frame show #f)))))

; Show the frame by calling its show method
(send frame show #t)
