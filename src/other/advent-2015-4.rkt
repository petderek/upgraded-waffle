#lang racket
(require openssl/md5)
(md5 (open-input-string "abcdef609043"))

(define (mine key)
  (for/fold
    ([answer ""])
    ([i (in-naturals)]
    #:break (or (> i 10000000) (string-prefix? answer "000000")))
  (~a (md5 (open-input-string (~a key i))) " " i)))

(time (mine "bgvyzdsv"))