#lang racket/base

(provide add-catalogs)

(define (add-catalogs run cats)
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (run #:quiet? #t
         "pkg" "config" "catalogs"))
  (define catalogs
    (let ([i (open-input-bytes (get-output-bytes o))])
      (for/list ([n (in-naturals)])
        (define l (read-line i))
        #:break (eof-object? l)
        l)))
  (unless (for/and ([cat (in-list cats)])
            (member cat catalogs))
    (apply run #:quiet? #t
           "pkg" "config" "--set" "-i" "catalogs"
           (list-ref catalogs 0)
           (append cats
                   (list ""))))
  (list-ref catalogs 0))
