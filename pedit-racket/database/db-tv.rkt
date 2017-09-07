#lang racket

(require db)

(provide (struct-out tv) db-tv-get db-tv-get-by-text db-tv-add
         db-tv-rename db-tv-set-pre-chrs db-tv-set-post-chrs db-tv-set-sep-chrs db-tv-del)

(struct tv (id name pre-chrs post-chrs sep-chrs))

(define (val-or-#f v)
  (if (sql-null? v) #f v))

(define (row-to-tv row)
  (tv (val-or-#f (vector-ref row 0))
      (val-or-#f (vector-ref row 1))
      (val-or-#f (vector-ref row 2))
      (val-or-#f (vector-ref row 3))
      (val-or-#f (vector-ref row 4))))

(define (db-tv-get db tv-id)
  (let ((r (query-rows db "select TextVersionID, Name, PreChrs, PostChrs, SepChrs from TextVersion where TextVersionID=$1;" tv-id)))
    (if r
        (row-to-tv tv)
        'nil)))

(define (tv-rows-to-list rows)
  (if (empty? rows)
      '()
      (cons (row-to-tv (first rows)) (tv-rows-to-list (rest rows)))))

(define (db-tv-get-by-text db text-id)
  (let ((r (query-rows db "select TextVersionID, Name, PreChrs, PostChrs, SepChrs from TextVersion where TextID=$1;" text-id)))
    (if r
        (tv-rows-to-list r)
        #f)))
  
(define (db-tv-add db text-id name)
  (query-exec db "insert into TextVersion(TextID, Name) values($1, $2);" text-id name))

(define (db-tv-rename db tv-id name)
  (query-exec db "update TextVersion set Name=$1 where TextVersionID=$2;" name tv-id))

(define (db-tv-set-pre-chrs db tv-id pre-chrs)
  (query-exec db "update TextVersion set PreChrs=$1 where TextVersionID=$2;" pre-chrs tv-id))

(define (db-tv-set-post-chrs db tv-id post-chrs)
  (query-exec db "update TextVersion set PostChrs=$1 where TextVersionID=$2;" post-chrs tv-id))

(define (db-tv-set-sep-chrs db tv-id sep-chrs)
  (query-exec db "update TextVersion set SepChrs=$1 where TextVersionID=$2;" sep-chrs tv-id))

(define (db-tv-del db tv-id)
  (query-exec db "delete from TextVersion where TextVersionID=$1;" tv-id))
