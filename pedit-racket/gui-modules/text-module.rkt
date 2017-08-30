#lang racket/gui

(require "../database/pedit-db.rkt")

(provide show-text-module)

; Make a frame by instantiating the frame% class
(define frame (new frame% (label "Pedit - Text Module")))
 
(new message% (parent frame)
     (label "Pedit: Text Module"))

;; Language Panel
(define language-panel (new horizontal-panel% (parent frame)))

(define language-choice (new choice% (parent language-panel)
                             (min-width 300)
                             (label "Language")
                             (choices '())))

(new button% (parent language-panel)
             (label "Add")
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% (parent language-panel)
             (label "Remove")
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% (parent language-panel)
             (label "Rename")
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )



;; Text Panel
(define text-panel (new horizontal-panel% (parent frame)))

(define text-choice (new choice% (parent text-panel)
                         (min-width 300)
                         (label "Text")
                         (choices (list))))

(new button% [parent text-panel]
             [label "Add"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% [parent text-panel]
             [label "Remove"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% [parent text-panel]
             [label "Rename"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

;; Text Version Panel
(define tv-panel (new horizontal-panel% (parent frame)))

(new list-box% (parent tv-panel)
     (label "Text Version")
     (choices '()))

(new button% (parent tv-panel)
             (label "Add")
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

;; Exit Button
(new button% (parent frame)
             (label "Exit")
             (callback (lambda (button event)
                         (send frame show #f))))
 
; Show the frame by calling its show method
;(send frame show #t)

(define (insert-languages languages)
  (unless (empty? languages)
    (let* ((v (first languages))
           (id (vector-ref v 0))
           (name (vector-ref v 1)))
      (send language-choice append (format "~a: ~a" id name)))
    (insert-languages (rest languages))))


(define (show-text-module db)

  (send text-choice clear)

  ;; get texts from the database
  (send text-choice append "TEXT_1")
  (send text-choice append "TEXT_2")
  (send text-choice append "TEXT_3")
  (send text-choice append "TEXT_4")


  (send language-choice clear)
  (insert-languages (db-language-get-all db))

  (send frame show #t))

;(init-text-module '())

