#lang racket

(require db)

(provide db-node-get-first db-node-get-by-id db-node-get-list
         db-node-add-first db-node-insert-before db-node-add-last
         db-node-move-first db-node-move-before db-node-move-last
         db-node-del)

(define (db-node-get-first db text-id)
  (let ((rows (query-rows db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeToID from TextNodeConnection);" text-id)))
    (if (empty? rows)
        #f
        (vector-ref (first rows) 0))))
  
(define (db-node-get-next db node-id)
  (let ((rows (query-rows db "select TextNodeToID from TextNodeConnection where TextNodeFromID=$1;" node-id)))
    (if (empty? rows)
        #f
        (vector-ref (first rows) 0))))

(define (db-node-get-by-id db node-id)
  '<BODY>)

(define (db-node-get-tail db node-id)
  (let ((next (db-node-get-next db node-id)))
    (if next
        (cons next (db-node-get-tail db next))
        '())))

(define (db-node-get-list db text-id)
  (let ((first (db-node-get-first db text-id)))
    (if first
        (cons first (db-node-get-tail db first))
        '())))

(define (db-node-add-first db text-id)
  '<BODY>)

(define (db-node-insert-before db text-id node-id)
  '<BODY>)

(define (db-node-add-last db text-id)
  (define last-node-res (query-rows db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeFromID from TextNodeConnection);" text-id))
  (define last-node-id (if (empty? last-node-res) #f (vector-ref (first last-node-res) 0)))
  (define new-node-res (query db "insert into TextNode(TextID) values($1);" text-id))
  (define new-node-id (cdr (assoc 'insert-id (simple-result-info new-node-res))))
;;  (print (format "[{[new=~a;last=~a]}]" new-node-id last-node-id))
  (when (and new-node-id last-node-id)
    (query-exec db "insert into TextNodeConnection(TextNodeFromID, TextNodeToID) values($1, $2);"
                last-node-id
                new-node-id)))

(define (db-node-move-first db node-id)
  '<BODY>)

(define (db-node-move-before db node-id node-id-before)
  '<BODY>)

(define (db-node-move-last db node-id)
  '<BODY>)

(define (db-node-del db node-id)
  (query-exec db "delete from TextNode where TextNodeID=$1;" node-id))