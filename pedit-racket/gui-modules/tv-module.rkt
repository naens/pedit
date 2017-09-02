#lang racket/gui

(require "../database/pedit-db.rkt")

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
                                'skip))))

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

(new message% (parent text-edit-hpanel)
     (label "tvs"))

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

(new message% (parent text-edit-panel)
     (label "[x] display permutations"))

(define (tvs-ckeckbox-list tvs)
  '<SKIP>)

(send frame show #t)
