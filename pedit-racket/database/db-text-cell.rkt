#lang racket

(require db)

(provide db-text-cell-get db-text-cell-set)

(define (db-text-cell-get db tv-id node-id)
  (let ((row (query-maybe-row db "select Pre, Post from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (when row
      (let ((text (query-list db "select WordPart.Text from WordPart natural join TextItem natural join TextItemTextVersion where TextItem.TextNodeID=0 and TextItemTextVersion.TextItemID=1 order by WordPart.TI_O;")))
        (values (vector-ref row 0) (apply string-append text) (vector-ref row 1))))))
 
(define (db-text-cell-set db tv-id node-id pre text post)
  ;; set pre/post to cell
  (let ((row (query-maybe-row db "select Pre, Post from TextCell where TextVersionID=$1 and TextNodeID=$2;" tv-id node-id)))
    (if row
        (query-exec db "update TextCell set Pre=$1, Post=$2 where TextVersionID=$1 and TextNodeID=$2;" pre post tv-id node-id)
        (query-exec db "insert into TextCell (TextVersionID, TextNodeID, Pre, Post) values ($1, $2, $3, $4);" tv-id node-id pre post)))
    ;; TODO: if wp exists, ???
             otherwise: create new wp
;;    ;; check if wp (tv-id/nodde-id) exists, is so delete its words (!
;;    ;; delete word parts in other cell containing parts of words to be
;;    ;; deleted
;;    (db-text-cell-create-wp db tv-id node-id text)

;; PREVIOUS VERSION
;;    (let ((ti-id (memf (lambda (id)
;;                         (equal? (db-text-item-get-string db id) text))
;;                       tis)))
;;      (print (format "{ti-id=~a}~%" ti-id))
;;      (if ti-id
;;          (db-connect-ti-tv db tv-id ti-id)           ; if found: connect tv to this text item
;;          (db-text-item-create db tv-id node-id text)))  ; if not found: create new and connect
;;         ))

))


(define (db-text-cell-get-string db tv-id node-id)
  (let ((wps (query-list db "select Text from WordPart where TextVersionID=$1 and TextNodeID=$2 order by TI_O;" tv-id node-id)))
    (list->string wps)))


(define (db-text-cell-create-wp db tv-id node-id text)
  (define word-id (db-last-id db (query db "insert into Word values (NULL);")))
  (query-exec db "insert into WordPart (TextVersionID, TextNodeID, WordID, TI_O, Text) values ($1, $2, $3, $4);" tv-id node-id word-id 0 text))
