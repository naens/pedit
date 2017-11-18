#lang racket/gui

(require "../database/pedit-db.rkt")

(require "tv-module.rkt")

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
                             (callback
                              (lambda (choice event)
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
        (first (string-split r ": "))
        #f)))

(define (get-current-lang-name)
  (let ((r (send language-choice get-string-selection)))
    (if r
        (second (string-split r ": "))
        #f)))

(new button% (parent language-panel)
     (label "Remove")
     (callback (lambda (button event)
                 (define lang-id (get-current-lang-id))
                 (when lang-id
                   (define r (message-box "Remove Language"
                                          (format "Do you wish to remove Language ~a?"
                                                  (get-current-lang-name))
                                          #f
                                          '(yes-no)))
                   (when (equal? r 'yes)
                     (db-lang-del db lang-id)
                     (redisplay-languages)
                     (redisplay-texts)
                     (redisplay-tvs))))))

(new button% (parent language-panel)
     (label "Rename")
     (callback (lambda (button event)
                 (define lang-id (get-current-lang-id))
                 (when lang-id
                   (define r (get-text-from-user "Rename Language"
                                                 "New Language name: "
                                                 frame
                                                 (get-current-lang-name)))
                   (when r
                     (db-lang-rename db lang-id r)
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
                 (define lang-id (get-current-lang-id))
                 (when lang-id
                   (define r (get-text-from-user "Add Text" "Add new Text: "))
                   (when r
                     (db-text-add db lang-id r)
                     (redisplay-texts)
                     (send text-choice set-selection
                           (- (send text-choice get-number) 1))
                     (redisplay-tvs))))))

(define (get-current-text-id)
  (let ((r (send text-choice get-string-selection)))
    (if r
        (first (string-split r ": "))
        #f)))

(define (get-current-text-name)
  (let ((r (send text-choice get-string-selection)))
    (if r
        (second (string-split r ": "))
        #f)))

(new button% (parent text-panel)
     (label "Remove")
     (callback (lambda (button event)
                 (define text-id (get-current-text-id))
                 (when text-id
                   (define r (message-box "Remove Text"
                                          (format "Do you wish to remove Text ~a?"
                                                  (get-current-text-name))
                                          #f
                                          '(yes-no)))
                   (when (equal? r 'yes)
                     (db-text-del db text-id)
                     (redisplay-texts)
                     (redisplay-tvs))))))

(new button% (parent text-panel)
     (label "Rename")
     (callback (lambda (button event)
                 (define text-id (get-current-text-id))
                 (when text-id
                   (define r (get-text-from-user "Rename Text"
                                                 "New Text name: "
                                                 frame
                                                 (get-current-text-name)))
                   (when r
                     (db-text-rename db text-id r)
                     (redisplay-texts))))))

;; Text Version Panel
(define tv-panel (new vertical-panel% (parent frame)))

(define tv-list-box (new list-box% (parent tv-panel)
                         (label "Text Version")
                         (choices '())
                         (style '(multiple column-headers clickable-headers ))
                         (columns '("id" "name" "pre" "post" "sep"))))

(define tv-btns-panel (new horizontal-panel% (parent tv-panel)))

(new button% (parent tv-btns-panel)
     (label "Add")
     (callback (lambda (button event)
                 (define text-id (get-current-text-id))
                 (when text-id
                   (define r (get-text-from-user
                              "Add Text Version"
                              "Add new Text Version: "))
                   (when r
                     (db-tv-add db text-id r)
                     (redisplay-tvs))))))

(define (get-current-tv)
  (let ((r (send tv-list-box get-selections)))
    (if r
        (send tv-list-box get-data (first r))
        #f)))

(new button% (parent tv-btns-panel)
     (label "Remove")
     (callback (lambda (button event)
                 (define tv-id_ (tv-id (get-current-tv)))
                 (when tv-id_
                   (define r (message-box "Remove Text Version"
                                          (format "Do you wish to remove Text Version ~a?"
                                                  (tv-name (get-current-tv)))
                                          #f
                                          '(yes-no)))
                   (when (equal? r 'yes)
                     (db-tv-del db tv-id_)
                     (redisplay-tvs))))))

(new button% (parent tv-btns-panel)
     (label "Rename")
     (callback (lambda (button event)
                 (define tv-id_ (tv-id (get-current-tv)))
                 (when tv-id_
                   (define r (get-text-from-user "Rename Text Version"
                                                 "New Text Version name: "
                                                 frame
                                                 (tv-name (get-current-tv))))
                   (when r
                     (db-tv-rename db tv-id_ r)
                     (redisplay-tvs))))))

(new button% (parent tv-btns-panel)
     (label "PreChrs")
     (callback (lambda (button event)
                 (define struct-tv (get-current-tv))
                 (define id (tv-id struct-tv))
                 (define v (tv-pre-chrs (get-current-tv)))
                 (when id
                   (define r (get-text-from-user "" "" frame (if v v "")))
                   (when r
                     (db-tv-set-pre-chrs db id r)
                     (send tv-list-box set-data (send tv-list-box get-selection)
                           (struct-copy tv struct-tv (pre-chrs r)))
                     (redisplay-tvs))))))

(new button% (parent tv-btns-panel)
     (label "PostChrs")
     (callback (lambda (button event)
                 (define struct-tv (get-current-tv))
                 (define id (tv-id struct-tv))
                 (define v (tv-post-chrs (get-current-tv)))
                 (when id
                   (define r (get-text-from-user "" "" frame (if v v "")))
                   (when r
                     (db-tv-set-post-chrs db id r)
                     (send tv-list-box set-data (send tv-list-box get-selection)
                           (struct-copy tv struct-tv (post-chrs r)))
                     (redisplay-tvs))))))

(new button% (parent tv-btns-panel)
     (label "SepChrs")
     (callback (lambda (button event)
                 (define struct-tv (get-current-tv))
                 (define id (tv-id struct-tv))
                 (define v (tv-sep-chrs (get-current-tv)))
                 (when id
                   (define r (get-text-from-user "" "" frame (if v v "")))
                   (when r
                     (db-tv-set-sep-chrs db id r)
                     (send tv-list-box set-data (send tv-list-box get-selection)
                           (struct-copy tv struct-tv (sep-chrs r)))
                     (redisplay-tvs))))))

;; Show Text Button
(new button% (parent frame)
     (label "Show Text")
     (callback (lambda (button event)
                 (show-tv-module db (get-current-text-id)))))

;; Exit Button
(new button% (parent frame)
     (label "Exit")
     (callback (lambda (button event)
                 (send frame show #f))))

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
    (let* ((tv (first tvs))
           (id (number->string (tv-id tv)))
           (name (tv-name tv))
           (pre-chrs (tv-pre-chrs tv))
           (post-chrs (tv-post-chrs tv))
           (sep-chrs (tv-sep-chrs tv))
           (number (send tv-list-box get-number)))
      (send tv-list-box append id)
      (send tv-list-box set-data number tv)
      (when name (send tv-list-box set-string number name 1))
      (when pre-chrs (send tv-list-box set-string number pre-chrs 2))
      (when post-chrs (send tv-list-box set-string number post-chrs 3))
      (when sep-chrs (send tv-list-box set-string number sep-chrs 4)))
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
