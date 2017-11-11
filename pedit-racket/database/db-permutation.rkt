#lang racket

(require db)
(require "db-common.rkt")

(provide db-permutation-src db-permutation-dest db-permutation-set db-permutation-del)

(define (db-permutation-src db tv-id node-id)
  (let ((row (query-maybe-row db "select TextVersionFromID, TextNodeFromID from Permutation where TextVersionToID=$1, TextNodeToID=$2;" tv-id node-id)))
    (if row
        (values (vector-ref row 0) (vector-ref row 1))
        (values tv-id node-id))))

(define (db-permutation-dest db tv-id node-id)
  (let ((row (query-maybe-row db "select TextVersionToID, TextNodeToID from Permutation where TextVersionFromID=$1, TextNodeFromID=$2;" tv-id node-id)))
    (if row
        (values (vector-ref row 0) (vector-ref row 1))
        (values tv-id node-id))))

(define (db-permutation-set db f-tv-id f-node-id t-tv-id t-node-id)
  (let-values (((ff-tv-id ff-node-id) (db-permutation-src db f-tv-id f-node-id))
               ((tt-tv-id tt-node-id) (db-permutation-dest db t-tv-id t-node-id)))
    (query-exec db "delete from Permutation where TextVersionFromID=$1 and TextNodeFromID=$2;" t-tv-id t-node-id)
    (query-exec db "delete from Permutation where TextVersionToID=$1 and TextNodeToID=$2;" f-tv-id f-node-id)
    (query-exec db "insert into Permutation (TextVersionFromID, TextNodeFromID, TextVersionToID, TextNodeToID) values ($1, $2, $3, $4);"
                ff-tv-id ff-node-id t-tv-id t-node-id)
    (query-exec db "insert into Permutation (TextVersionFromID, TextNodeFromID, TextVersionToID, TextNodeToID) values ($1, $2, $3, $4);"
                tt-tv-id tt-node-id f-tv-id f-node-id)))

(define (db-permutation-del db tv-id node-id)
  (let-values (((f-tv-id f-node-id) (db-permutation-src db tv-id node-id))
               ((t-tv-id t-node-id) (db-permutation-dest db tv-id node-id)))
    (query-exec db "insert into Permutation (TextVersionFromID, TextNodeFromID, TextVersionToID, TextNodeToID) values ($1, $2, $3, $4);"
                f-tv-id f-node-id t-tv-id t-node-id)
    (query-exec db "delete from Permutation where TextVersionFromID=$1 and TextNodeFromID=$2;" tv-id node-id)
    (query-exec db "delete from Permutation where TextVersionToID=$1 and TextNodeToID=$2;" tv-id node-id)))
