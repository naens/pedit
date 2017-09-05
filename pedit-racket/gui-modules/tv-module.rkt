#lang racket/gui

(require table-panel)

(require "../database/pedit-db.rkt")

(require "../gui-classes/text-cell.rkt")

(provide show-tv-module)

(define frame (new frame% (label "Pedit - Text Version Module")))

(new message% (parent frame)
     (label "Pedit: Text Version Module"))

;; Label Panel
(define label-panel (new horizontal-panel% (parent frame)
                         (stretchable-width #t)
                         ))

(define label-choice (new choice% (parent label-panel)
                             (min-width 80)
                             (stretchable-width #t)
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
(define text-edit-panel (new vertical-panel% (parent frame)
                             (stretchable-width #t)))

(define text-edit-hpanel (new horizontal-panel% (parent text-edit-panel)
                             (stretchable-width #t)))

;; tvs check boxes
(define tvs-check-box-panel (new vertical-panel% (parent text-edit-hpanel)
                                 
                       (stretchable-width #f)))

;; hpanel needed for the table in order to make it scrollable...
(define tvs-table-hpanel (new horizontal-panel% (parent text-edit-hpanel)
                       (style '(auto-hscroll auto-vscroll))
                       (stretchable-width #t)
                       (min-width 100)))

;; table [y:tvs,x:nodes]
(define tvs-table (new table-panel% (parent tvs-table-hpanel)
                       (dimensions '(4 7))
                       (column-stretchability #f)
                       (row-stretchability #f)
                       (stretchable-width #t)))

(for ((j '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
  (new text-cell% (parent tvs-table)
       (pre " [PRE]")
       (text (symbol->string j))
       (post "[POST] ")
       ))

;; buttons
(define text-edit-button-panel (new vertical-panel% (parent text-edit-hpanel)
                                    
                       (stretchable-width #f)))

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

;; permutations
(new check-box% (parent text-edit-panel)
     (stretchable-width #t)
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
