#lang racket/gui

(require "../database/pedit-db.rkt")

(provide show-tv-module)

(define frame (new frame% (label "Pedit - Text Version Module")))

(new message% (parent frame)
     (label "Pedit: Text Version Module"))

;; Label Panel
(define label-panel (new horizontal-panel% (parent frame)))

(define label-choice (new choice% (parent label-panel)
                             (min-width 300)
                             (label "Label")
                             (choices '())
                             (callback
                              (lambda (choice event)
                                'TODO))))

(new button% (parent label-panel)
             (label "Add")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent label-panel)
             (label "Remove")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent label-panel)
             (label "Rename")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent label-panel)
             (label "Go")
             (callback (lambda (button event)
                         'skip)))

;; Text Edit Panel
(define text-edit-panel (new vertical-panel% (parent frame)))

(define text-edit-hpanel (new horizontal-panel% (parent text-edit-panel)))

(define tvs-check-box-panel (new vertical-panel% (parent text-edit-hpanel)))

(new message% (parent text-edit-hpanel)
     (label "table"))

(define text-edit-button-panel (new vertical-panel% (parent text-edit-hpanel)))

(new button% (parent text-edit-button-panel)
             (label "Insert Node")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent text-edit-button-panel)
             (label "Edit Text Item")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent text-edit-button-panel)
             (label "Remove Node")
             (callback (lambda (button event)
                         'skip)))

(new button% (parent text-edit-button-panel)
             (label "Add Permutation")
             (callback (lambda (button event)
                         'skip)))

(new check-box% (parent text-edit-panel)
     (label "Display Permutations"))

(define tvs 'nil)

(define tv-cb% (class check-box%
                 (init tv)
                 (super-new) 
                 (define current-tv tv)
                 (define/public (get-tv) current-tv)))

(define (create-tv-check-box tv)
  (new tv-cb% (tv tv) (parent tvs-check-box-panel)
                 (label (tv-name tv))))

(define (add-tvs tvs_)
  (set! tvs tvs_)
  (map (lambda (tv)
        (create-tv-check-box tv))
       tvs))

(define (clear-tvs-panel)
  (map (lambda (cb)(send tvs-check-box-panel delete-child cb))
       (send tvs-check-box-panel get-children)))

(define (redisplay-tvs text-id)
  (clear-tvs-panel)
  (add-tvs (db-tv-get-by-text db text-id)))

(define db 'nil)
(define (show-tv-module db_ text-id)
  (set! db db_)
  (redisplay-tvs text-id)
  (send frame show #t))

;; Exit Button
(new button% (parent frame)
             (label "Exit")
             (callback (lambda (button event)
                         (send frame show #f))))
