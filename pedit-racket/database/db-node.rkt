#lang racket

(require db)

(provide db-node-get-first db-node-get-by-id
         db-node-add-first db-node-append
         db-node-move-first db-node-move-before db-node-move-last
         db-node-del)

(define (db-node-get-first text-id) '<BODY>)
(define (db-node-get-by-id node-id) '<BODY>)
(define (db-node-add-first text-id) '<BODY>)
(define (db-node-append node-id) '<BODY>)
(define (db-node-move-first node-id) '<BODY>)
(define (db-node-move-before node-id node-id-before) '<BODY>)
(define (db-node-move-last node-id) '<BODY>)
(define (db-node-del node-id) '<BODY>)
