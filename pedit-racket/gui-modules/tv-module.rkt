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
                                ;TODO get labels from database in order of nodes
                                'TODO))))

(new button% (parent label-panel)
     (label "Add")
     (callback (lambda (button event)
                 (when text-cell
                   (define r (get-text-from-user
                              "Add Label to Node"
                              "New Label name: "))
                   (when r
                     (db-label-add db (send text-cell get-node) r))))))

(new button% (parent label-panel)
     (label "Delete")
     (callback (lambda (button event)
                 'skip)))

(new button% (parent label-panel)
     (label "Rename")
     (callback (lambda (button event)
                 'skip)))

(new button% (parent label-panel)
     (label "Move")
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

;; tvs row-labels
(define tvs-active-panel (new vertical-panel% (parent text-edit-hpanel)
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

;; buttons
(define text-edit-button-panel (new vertical-panel% (parent text-edit-hpanel)
                                    
                       (stretchable-width #f)))

(new button% (parent text-edit-button-panel)
     (label "Append Node")
     (stretchable-width #t)
     (callback (lambda (button event)
                 (let ((tv-id (if text-cell (tv-id (send text-cell get-tv)) #f)))
                   (redisplay-nodes tv-id (db-node-add-last db text-id))))))

(new button% (parent text-edit-button-panel)
     (label "Insert Node")
     (stretchable-width #t)
     (callback (lambda (button event)
                 (let ((new-node (if text-cell
                                     (db-node-insert-before db text-id (send text-cell get-node))
                                     (db-node-add-first db text-id)))
                       (tv-id (if text-cell (tv-id (send text-cell get-tv)) #f)))
                   (redisplay-nodes tv-id new-node)))))

;; TODO: get existing value
;; TODO: concatenate -> prefilled dialog value
;; DONE: display dialog with edit field
;; DONE: based on pre/post values of tv, separate into pre/text/post
;; TODO: insert value into the database

;; separate string in pre/text/post, return 3 values
;; pre is made of pre-chrs and sep-chrs
;; post is made of post-chrs and sep-chrs
;; text is the part between pre and post
(define (split-text-cell-string string pre-chrs post-chrs sep-chrs)
  (let ((pre-set (set-union pre-chrs sep-chrs))
        (post-set (set-union sep-chrs post-chrs)))
    (let*-values (((pre rest1) (splitf-at string
                                          (lambda (c)
                                            (set-member? pre-set c))))
                  ((text rest2) (splitf-at rest1
                                           (lambda (c)
                                             (not (set-member? post-set c)))))
                  ((post rest3) (splitf-at rest2
                                           (lambda (c)
                                             (set-member? post-set c)))))
      (values (list->string pre) (list->string text) (list->string post)))))

(define (set-text-cell-text text-cell string)
  (let ((tv-id (tv-id (send text-cell get-tv)))
        (node-id (send text-cell get-node)))
    (let*-values (((pre-chrs post-chrs sep-chrs) (db-tv-get-seps db tv-id))
                  ((pre text post) (split-text-cell-string (string->list string) pre-chrs post-chrs sep-chrs)))
      (db-text-cell-set db tv-id node-id pre text post))))

(new button% (parent text-edit-button-panel)
     (label "Edit Text Cell")
     (stretchable-width #t)
     (callback (lambda (button event)
                 (when text-cell
                   (let* ((tv-id (tv-id (send text-cell get-tv)))
                          (node-id (send text-cell get-node))
                          (r (get-text-from-user "Modify Text Cell"
                                                 "New Text Cell value: "
                                                 frame
                                                 (db-text-cell-get-text db tv-id node-id))))
                     (when r
                       (set-text-cell-text text-cell r)
                       (redisplay-nodes)))))))

(new button% (parent text-edit-button-panel)
     (label "Delete Node")
     (stretchable-width #t)
     (callback (lambda (button event)
                 (when text-cell
                   (define node-id (send text-cell get-node))
                   (define r (message-box "Delete Node"
                                          "Delete current node?"
                                          #f
                                          '(yes-no)))
                   (when (equal? r 'yes)
                     (let ((node-after (db-node-get-after db node-id)))
                       (let ((node-sel (if node-after node-after (db-node-get-before db node-id))))
                         (db-node-del db text-id node-id)
                         (redisplay-nodes (tv-id (send text-cell get-tv)) node-sel))))))))

(new button% (parent text-edit-button-panel)
     (label "Add Permutation")
     (stretchable-width #t)
     (callback (lambda (button event)
                 'skip)))

(define (show-permutations-p)
  (send permutations-cb get-value))

;; permutations
(define permutations-cb
  (new check-box% (parent text-edit-panel)
     (stretchable-width #t)
     (value #t)
     (label "Display Permutations")))

(define tvs '())

(define tv-cb% (class check-box%
                 (init tv)
                 (super-new) 
                 (define current-tv tv)
                 (define/public (get-tv) current-tv)))

(define (create-tv-check-box tv)
  (new tv-cb% (tv tv) (parent tvs-check-box-panel)
                 (label (tv-name tv))
                 (callback (lambda (cb event)
                             (redisplay-active-tvs)
                             (redisplay-nodes)))     
                 (value #t)))

(define (add-tvs tvs_)
  (set! tvs tvs_)
  (map (lambda (tv)
        (create-tv-check-box tv))
       tvs))

(define (delete-children panel)
  (map (lambda (child) (send panel delete-child child))
       (send panel get-children)))

(define (clear-tvs-panel)
  (delete-children tvs-check-box-panel))

(define (redisplay-tvs)
  (clear-tvs-panel)
  (add-tvs (db-tv-get-by-text db text-id)))

(define (display-active-tv tv)
  (new message% (parent tvs-active-panel)
       (label (tv-name tv))))

(define (clear-active-tvs)
  (delete-children tvs-active-panel))

(define (redisplay-active-tvs)
  (clear-active-tvs)
  (map (lambda (tv)
         (display-active-tv tv))
       (get-active-tvs)))

(define (get-active-tvs)
  (filter-map (lambda (cb)
                (if (send cb get-value)
                    (send cb get-tv)
                    #f))
              (send tvs-check-box-panel get-children)))

(define text-cell #f)

(define (redisplay-nodes (sel-tv-id #f) (sel-node-id #f))
  (delete-children tvs-table)
  (let ((active-tvs (get-active-tvs))
        (node-list (db-node-get-list db text-id))
        (sel-tv (if sel-tv-id
                    sel-tv-id
                    (if text-cell (tv-id (send text-cell get-tv)) #f)))
        (sel-node (if sel-node-id
                      sel-node-id
                      (if text-cell (send text-cell get-node) #f))))
    (set! text-cell #f)
    (when (and (> (length active-tvs) 0) (> (length node-list) 0))
      (send tvs-table set-dimensions (length node-list) (length active-tvs))
      (for ((tv active-tvs))
        (for ((node node-list))
          (let ((tc (new text-cell% (parent tvs-table)
                         (pre (db-text-cell-get-pre db (tv-id tv) node))
                                  ; TODO: display permutations
                         (text (db-text-cell-get-text db (tv-id tv) node))
                         (post (db-text-cell-get-post db (tv-id tv) node))
                         (tv tv)
                         (node node)
                         (on-cell-click
                          (lambda (text-cell_)
                            (when text-cell
                              (send text-cell unselect))
                            (set! text-cell text-cell_)
                            (send text-cell select)))
                         (on-cell-cclick
                          (lambda (text-cell_)
                            (when text-cell
                              (let ((tv-id (tv-id (send text-cell get-tv)))
                                    (f-node-id (send text-cell get-node))
                                    (t-node-id (send text-cell_ get-node)))
                                (when (not (= f-node-id t-node-id))
                                 ;; (print (format "[permutation:~a->~a]" f-node-id t-node-id))
                                  (db-permutation-set  db tv-id f-node-id t-node-id)))))))))
            (when (and (and sel-node (equal? (send tc get-node) sel-node))
                       (and sel-tv (equal? (tv-id (send tc get-tv)) sel-tv)))
              (set! text-cell tc)
              (send text-cell select))))))))

(define db 'nil)
(define text-id 'nil)

(define (show-tv-module db_ text-id_)
  (set! db db_)
  (set! text-id text-id_)
  (redisplay-tvs)
  (redisplay-active-tvs)
  (redisplay-nodes)
  (send frame show #t))

;; Exit Button
(new button% (parent frame)
     (label "Exit")
     (callback (lambda (button event)
                 (send frame show #f))))
