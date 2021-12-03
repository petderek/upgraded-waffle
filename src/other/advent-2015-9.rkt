#lang racket

;; basic idea here -- graph is a hash of key: name, value: node
;; node has a name (= hash key), and a hash of key: name (of other node), value: distance

(define graph (make-hash))

(struct node (name edges))

(define (line->hash line)
  (let [(keys (string-split line))]
    (hash-update!
     graph
     (first keys)
     identity
     (node (first keys) (make-hash)))
    (hash-set!
     (node-edges (hash-ref graph (first keys)))
     (third keys)
     (string->number (last keys)))))
     

;; load graph  
(with-input-from-file "advent-2015-9.txt"
  (lambda ()
    (for ([line (in-lines)])
      (line->hash line))))

