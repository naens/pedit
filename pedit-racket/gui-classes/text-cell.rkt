#lang racket/gui

(provide text-cell%)

(define text-cell%
  (class horizontal-panel%
    (init pre text post)
    (super-new)

    (send this border 1)

    (define current-pre pre)
    (define current-text text)
    (define current-post post)

    (define/public (get-pre) current-pre)
    (define/public (get-text) current-text)
    (define/public (get-post) current-post)

    (define pre-label (new message% (parent this) (label pre)))
    (define text-label (new message% (parent this) (label text)))
    (define post-label (new message% (parent this) (label post)))

    (define/public (set-pre new-pre)
      (set! current-pre new-pre)
      (send pre-label set-label new-pre))

    (define/public (set-text new-text)
      (set! current-text new-text)
      (send text-label set-label new-text))

    (define/public (set-post new-post)
      (set! current-post new-post)
      (send post-label set-label new-post))))
