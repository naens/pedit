#lang racket/gui

(require "../database/pedit-db.rkt")

(provide text-cell%)

(define text-cell%
  (class horizontal-panel%
    (init pre text post tv node on-cell-click on-cell-cclick)
    (super-new)

    (send this border 1)

    (define current-pre pre)
    (define current-text text)
    (define current-post post)
    (define current-tv tv)
    (define current-node node)
    (define current-on-cell-click on-cell-click)
    (define current-on-cell-cclick on-cell-cclick)

    (define/public (get-pre) current-pre)
    (define/public (get-text) current-text)
    (define/public (get-post) current-post)
    (define/public (get-tv) current-tv)
    (define/public (get-node) current-node)

    (define pre-label (new message% (parent this) (label pre) (enabled #f)))
    (define text-label (new message% (parent this) (label text) (enabled #f)))
    (define post-label (new message% (parent this) (label post) (enabled #f)))

    (define/public (set-pre new-pre)
      (set! current-pre new-pre)
      (send pre-label set-label new-pre))

    (define/public (set-text new-text)
      (set! current-text new-text)
      (send text-label set-label new-text))

    (define/public (set-post new-post)
      (set! current-post new-post)
      (send post-label set-label new-post))

    (define/public (select)
      (send pre-label enable #t)
      (send text-label enable #t)
      (send post-label enable #t))

    (define/public (unselect)
      (send pre-label enable #f)
      (send text-label enable #f)
      (send post-label enable #f))

    (define/override (on-subwindow-event receiver event)
      (super on-subwindow-event receiver event)
      (when (equal? (send event get-event-type) 'left-up)
        (if (send event get-control-down)
            (current-on-cell-cclick this)        
            (current-on-cell-click this))))))
