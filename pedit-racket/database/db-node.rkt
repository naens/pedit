#lang racket

(require db)

(require "db-common.rkt")

(require "db-permutation.rkt")

(require "db-tv.rkt")

(provide db-node-get-first db-node-get-by-id db-node-get-list db-node-get-before db-node-get-after
         db-node-add-first db-node-insert-before db-node-add-last
         db-node-move-first db-node-move-before db-node-move-last
         db-node-del)

(define (db-node-get-first db text-id)
  (query-maybe-value db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeToID from TextNodeConnection);" text-id))

(define (db-node-get-before db node-id)
  (query-maybe-value db "select TextNodeFromID from TextNodeConnection where TextNodeToID=$1;" node-id))

(define (db-node-get-after db node-id)
  (query-maybe-value db "select TextNodeToID from TextNodeConnection where TextNodeFromID=$1;" node-id))

(define (db-node-get-by-id db node-id)
  '<BODY>)

(define (db-node-get-tail db node-id)
  (let ((next (db-node-get-after db node-id)))
    (if next
        (cons next (db-node-get-tail db next))
        '())))

(define (db-node-get-list db text-id)
  (let ((first (db-node-get-first db text-id)))
    (if first
        (cons first (db-node-get-tail db first))
        '())))

(define (db-node-add-first db text-id)
  (let ((old-first (db-node-get-first db text-id))
        (new-node-id (db-last-id db (query db "insert into TextNode(TextID) values($1);" text-id))))
    (when old-first
      (query-exec db "insert into TextNodeConnection (TextNodeFromID, TextNodeToID) values ($1, $2);" new-node-id old-first))
    new-node-id))

(define (db-node-insert-before db text-id node-id)
  (let ((node-before (db-node-get-before db node-id))
        (new-node-id (db-last-id db (query db "insert into TextNode(TextID) values($1);" text-id))))
    (when node-before
      (query-exec db "insert into TextNodeConnection (TextNodeFromID, TextNodeToID) values ($1, $2);" node-before new-node-id)
      (query-exec db "delete from TextNodeConnection where TextNodeFromID=$1 and TextNodeToID=$2" node-before node-id))
    (query-exec db "insert into TextNodeConnection (TextNodeFromID, TextNodeToID) values ($1, $2);" new-node-id node-id)
    new-node-id))

(define (db-node-add-last db text-id)
  (define last-node-res (query-rows db "select TextNodeID from TextNode where TextID=$1 and TextNodeID not in (select TextNodeFromID from TextNodeConnection);" text-id))
  (define last-node-id (if (empty? last-node-res) #f (vector-ref (first last-node-res) 0)))
  (define new-node-id (db-last-id db (query db "insert into TextNode(TextID) values($1);" text-id)))
 ;;  (print (format "[{[new=~a;last=~a]}]" new-node-id last-node-id))
  (when (and new-node-id last-node-id)
    (query-exec db "insert into TextNodeConnection(TextNodeFromID, TextNodeToID) values($1, $2);"
                last-node-id
                new-node-id))
  new-node-id)

(define (db-node-move-first db node-id)
  '<BODY>)

(define (db-node-move-before db node-id node-id-before)
  '<BODY>)

(define (db-node-move-last db node-id)
  '<BODY>)

(define (db-node-del db text-id node-id)
  (let ((node-before (db-node-get-before db node-id)))
    (when node-before
      (let ((node-after (db-node-get-after db node-id)))
        (when node-after
          (query-exec db "insert into TextNodeConnection (TextNodeFromID, TextNodeToID) values ($1, $2);" node-before node-after)))))
  (map (lambda (tv-id) (db-permutation-del db tv-id node-id))
       (db-tv-list-by-text db text-id))
  (query-exec db "delete from TextNode where TextNodeID=$1;" node-id))
