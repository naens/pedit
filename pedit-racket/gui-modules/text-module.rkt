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
                             (choices '())
                             (callback (lambda (choice event)
                                         (redisplay-texts)))))
                             

(new button% (parent language-panel)
             (label "Add")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Add Language" "Add new Language: ")))
                           (when r
                             (db-language-add db r)
                             (redisplay-languages)
                             (send language-choice set-selection
                                   (- (send language-choice get-number) 1))
                             (redisplay-texts))))))

(define (get-current-language-id)
  (first (string-split (send language-choice get-string-selection) ":")))

(define (get-current-language-name)
  (second (string-split (send language-choice get-string-selection) ": ")))

(new button% (parent language-panel)
             (label "Remove")
             (callback (lambda (button event)
                         (let* ((r (message-box "Remove Language"
                                                (format "Do you wish to remove Language ~a?"
                                                        (get-current-language-name))
                                                #f
                                                '(yes-no))))
                           ;;TODO confirm delete language texts, other objects (lemmas, categories...)
                           (when (equal? r 'yes)
                             (db-language-del db (get-current-language-id))
                             (db-text-del-by-language db (get-current-language-id))
                             (redisplay-languages)
                             (redisplay-texts))))))

(new button% (parent language-panel)
             (label "Rename")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Rename Language"
                                                      "New Language name: "
                                                      frame
                                                      (get-current-language-name))))
                           (when r
                             (db-language-rename db (get-current-language-id) r)
                             (redisplay-languages))))))

;; Text Panel
(define text-panel (new horizontal-panel% (parent frame)))

(define text-choice (new choice% (parent text-panel)
                         (min-width 300)
                         (label "Text")
                         (choices (list))))

(new button% (parent text-panel)
             (label "Add")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Add Text" "Add new Text ")))
                           (when r
                             (db-text-add db (get-current-language-id) r)
                             (redisplay-texts)
                             (send text-choice set-selection
                                   (- (send text-choice get-number) 1)))))))

(define (get-current-text-id)
  (first (string-split (send text-choice get-string-selection) ":")))

(define (get-current-text-name)
  (second (string-split (send text-choice get-string-selection) ": ")))

(new button% (parent text-panel)
             (label "Remove")
             (callback (lambda (button event)
                         (let* ((r (message-box "Remove Text"
                                                (format "Do you wish to remove Text ~a?"
                                                        (get-current-text-name))
                                                #f
                                                '(yes-no))))
                           ;;TODO confirm delete text versions, nodes...
                           (when (equal? r 'yes)
                             (db-text-del db (get-current-text-id))
                             (redisplay-texts))))))

(new button% (parent text-panel)
             (label "Rename")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Rename Text"
                                                      "New Text name: "
                                                      frame
                                                      (get-current-text-name))))
                           (when r
                             (db-text-rename db (get-current-text-id) r)
                             (redisplay-texts))))))

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

(define (redisplay-languages)
  (define index (send language-choice get-selection))
  (send language-choice clear)
  (insert-languages (db-language-get-all db))
  (define n (send language-choice get-number))
  (when (and index n)
    (send language-choice set-selection (min (- n 1) index))))

(define (insert-texts texts)
  (unless (empty? texts)
    (let* ((v (first texts))
           (id (vector-ref v 0))
           (name (vector-ref v 1)))
      (send text-choice append (format "~a: ~a" id name)))
    (insert-texts (rest texts))))

(define (redisplay-texts)
  (define index (send text-choice get-selection))
  (send text-choice clear)
  (insert-texts (db-text-get-by-language db (get-current-language-id)))
  (define n (send text-choice get-number))
  (when (and index n (> n 0))
    (send text-choice set-selection (min (- n 1) index))))

(define db 'nil)
(define (show-text-module db_)
  (set! db db_)

  (redisplay-languages)

  (redisplay-texts)

  (send frame show #t))

;(init-text-module '())
