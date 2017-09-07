#lang racket

(require db)

(provide db-node-get-first db-node-get-by-id
         db-node-add-first db-node-insert-before db-node-add-last
         db-node-move-first db-node-move-before db-node-move-last
         db-node-del)

(define (db-node-get-first db text-id)
  (query-rows db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeFromID from TextNodeConnection);"))


(define (db-node-get-by-id db node-id)
  '<BODY>)

(define (db-node-add-first db text-id)
  '<BODY>)

(define (db-node-insert-before db text-id node-id)
  '<BODY>)

(define (db-node-add-last db text-id)
  (define new-node-res (query db "insert into TextNode(TextID) values($1, $2);" text-id))
  (define last-node-res (query db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeToID from TextNodeConnection);"))
  (when (not (empty? last-node-res))
    (query-exec db "insert into TextNodeConnection(TextNodeFromID, TextNodeToID) values($1, $1);"
                last
                (cdr (assoc 'insert-id (simple-result-info new-node-res))))))

(define (db-node-move-first db node-id)
  '<BODY>)

(define (db-node-move-before db node-id node-id-before)
  '<BODY>)

(define (db-node-move-last db node-id)
  '<BODY>)

(define (db-node-del db node-id)
  (query-exec db "delete from TextNode where TextNodeID=$1;" node-id))