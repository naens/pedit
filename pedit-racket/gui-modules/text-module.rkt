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
                                         (redisplay-texts)
                                         (redisplay-tvs)))))
                             

(new button% (parent language-panel)
             (label "Add")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Add Language" "Add new Language: ")))
                           (when r
                             (db-lang-add db r)
                             (redisplay-languages)
                             (send language-choice set-selection
                                   (- (send language-choice get-number) 1))
                             (redisplay-texts)
                             (redisplay-tvs))))))

(define (get-current-lang-id)
  (let ((r (send language-choice get-string-selection)))
    (if r
        (first (string-split r ":"))
        #f)))

(define (get-current-lang-name)
  (let ((r (send language-choice get-string-selection)))
    (if r
        (second (string-split r ":"))
        #f)))

(new button% (parent language-panel)
             (label "Remove")
             (callback (lambda (button event)
                         (let* ((r (message-box "Remove Language"
                                                (format "Do you wish to remove Language ~a?"
                                                        (get-current-lang-name))
                                                #f
                                                '(yes-no))))
;; TODO confirm delete language texts, other objects (lemmas, categories...)
;; TODO disable button if no languages
                           (when (equal? r 'yes)
                             (db-lang-del db (get-current-lang-id))
                             (redisplay-languages)
                             (redisplay-texts)
                             (redisplay-tvs))))))

(new button% (parent language-panel)
             (label "Rename")
;; TODO disable button if no languages
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Rename Language"
                                                      "New Language name: "
                                                      frame
                                                      (get-current-lang-name))))
                           (when r
                             (db-lang-rename db (get-current-lang-id) r)
                             (redisplay-languages))))))

;; Text Panel
(define text-panel (new horizontal-panel% (parent frame)))

(define text-choice (new choice% (parent text-panel)
                         (min-width 300)
                         (label "Text")
                         (choices (list))
                         (callback (lambda (choice event)
                                     (redisplay-tvs)))))

(new button% (parent text-panel)
             (label "Add")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user "Add Text" "Add new Text: ")))
                           (when r
                             (db-text-add db (get-current-lang-id) r)
                             (redisplay-texts)
                             (send text-choice set-selection
                                   (- (send text-choice get-number) 1)))))))

(define (get-current-text-id)
  (let ((r (send text-choice get-string-selection)))
    (if r
        (first (string-split r ":"))
        #f)))

(define (get-current-text-name)
  (let ((r (send text-choice get-string-selection)))
    (if r
        (second (string-split r ":"))
        #f)))

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

(define tv-list-box (new list-box% (parent tv-panel)
                         (label "Text Version")
                         (choices '())))

(new button% (parent tv-panel)
             (label "Add")
             (callback (lambda (button event)
                         (let ((r (get-text-from-user
                                   "Add Text Version"
                                   "Add new Text Version: ")))
                           (when r
                             (db-tv-add db (get-current-text-id) r)
                             (redisplay-tvs)
                             (send text-choice set-selection
                                   (- (send text-choice get-number) 1)))))))

;; TODO: add tv-rename and tv-delete buttons
;; TODO: tv-edit module

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
  (insert-languages (db-lang-get-all db))
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
  (define lang-id (get-current-lang-id))
  (when lang-id
    (insert-texts (db-text-get-by-lang db lang-id))
    (define n (send text-choice get-number))
    (when (and index n (> n 0))
      (send text-choice set-selection (min (- n 1) index)))))

(define (insert-tvs tvs)
  (unless (empty? tvs)
    (let* ((v (first tvs))
           (id (vector-ref v 0))
           (name (vector-ref v 1)))
      (send tv-list-box append (format "~a: ~a" id name)))
    (insert-tvs (rest tvs))))

(define (redisplay-tvs)
  (define index (send tv-list-box get-selection))
  (send tv-list-box clear)
  (define text-id (get-current-text-id))
  (when text-id
    (insert-tvs (db-tv-get-by-text db text-id))
    (define n (send tv-list-box get-number))
    (when (and index n (> n 0))
      (send tv-list-box set-selection (min (- n 1) index)))))

(define db 'nil)
(define (show-text-module db_)
  (set! db db_)

  (redisplay-languages)

  (redisplay-texts)

  (redisplay-tvs)
  
  (send frame show #t))

;(init-text-module '())
