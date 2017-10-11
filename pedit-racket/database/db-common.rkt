#lang racket

(require db)

(provide db-last-id)

(define (db-last-id db insert-res)
  ;(print (format "(insert-res=~a) " insert-res))
  ;(cdr (assoc 'insert-id (simple-result-info insert-res)))
  (query-maybe-value db "SELECT last_insert_rowid();")
  )
