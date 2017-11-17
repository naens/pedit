#lang racket

(require db)
(require "db-common.rkt")

(provide db-permutation-src db-permutation-dest db-permutation-set db-permutation-del)

(define (db-permutation-src db tv-id node-id)
  (let ((src-node-id (query-maybe-value db "select TextNodeFromID from Permutation where TextVersionID=$1 and TextNodeToID=$2;" tv-id node-id)))
    (if src-node-id src-node-id node-id)))

(define (db-permutation-dest db tv-id node-id)
  (let ((dest-node-id (query-maybe-value db "select TextNodeToID from Permutation where TextVersionID=$1 and TextNodeFromID=$2;" tv-id node-id)))
    (if dest-node-id dest-node-id node-id)))

(define (db-permutation-set db tv-id f-node-id t-node-id)
  (let ((ff-node-id (db-permutation-src db tv-id f-node-id))
        (tt-node-id (db-permutation-dest db tv-id t-node-id)))
    (when (not (= f-node-id t-node-id))
      (query-exec db "delete from Permutation where TextVersionID=$1 and TextNodeFromID=$2;" tv-id t-node-id)
      (query-exec db "delete from Permutation where TextVersionID=$1 and TextNodeToID=$2;" tv-id f-node-id)
      (query-exec db "insert into Permutation (TextVersionID, TextNodeFromID, TextNodeToID) values ($1, $2, $3);" tv-id ff-node-id t-node-id)
      (query-exec db "insert into Permutation (TextVersionID, TextNodeFromID, TextNodeToID) values ($1, $2, $3);" tv-id tt-node-id f-node-id))))

(define (db-permutation-del db tv-id node-id)
  (let ((f-node-id (db-permutation-src db tv-id node-id))
        (t-node-id (db-permutation-dest db tv-id node-id)))
    (when (not (= f-node-id t-node-id))
      (query-exec db "insert into Permutation (TextVersionID, TextNodeFromID, TextNodeToID) values ($1, $2, $3);" tv-id f-node-id t-node-id))
    (query-exec db "delete from Permutation where TextVersionID=$1 and TextNodeFromID=$2;" tv-id node-id)
    (query-exec db "delete from Permutation where TextVersionID=$1 and TextNodeToID=$2;" tv-id node-id)))
