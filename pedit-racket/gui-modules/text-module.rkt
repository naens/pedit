#lang racket/gui

; Make a frame by instantiating the frame% class
(define frame (new frame% (label "Pedit - Text Module")))
 
(new message% (parent frame)
     (label "Pedit: Text Module"))

;; Text Panel
(define text-panel (new horizontal-panel% (parent frame)))
(new combo-field% (parent text-panel)
     (label "Text")
     (choices '()))

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

;; Language Panel
(define language-panel (new horizontal-panel% (parent frame)))
(new combo-field% (parent language-panel)
     (label "Language")
     (choices '()))

(new button% [parent language-panel]
             [label "Add"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% [parent language-panel]
             [label "Remove"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

(new button% [parent language-panel]
             [label "Rename"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

;; Text Version Panel
(define tv-panel (new horizontal-panel% (parent frame)))

(new list-box% (parent tv-panel)
     (label "Text Version")
     (choices '()))

(new button% [parent tv-panel]
             [label "Add"]
           ;  (callback (lambda (button event)
           ;              (send msg set-label "Right click")))
             )

;; Exit Button
(new button% (parent frame)
             (label "Exit")
             (callback (lambda (button event)
                         (send frame show #f))))
 
; Show the frame by calling its show method
(send frame show #t)
