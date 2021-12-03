#lang racket

(define (get-num-chars line)
  (string-length line))

(define (get-mem-chars line)
  (cond
    [(string=? "\"\"" line) 0]
    [else 
     (let loop [(head (second (string->list line)))
                (characters (drop (string->list line) 2))
                (sum 0)]
       (cond
         [(and (eq? #\" head) (empty? characters)) sum]
         [(not (eq? #\\ head))
          (loop (first characters) (rest characters) (add1 sum))]
         [(or (eq? #\\ (first characters)) (eq? #\" (first characters)))
          (loop (second characters) (rest (rest characters)) (add1 sum))]
         [(eq? #\x (first characters))
          (loop (fourth characters) (drop characters 4) (add1 sum))]
         [else
          (loop (first characters) (rest characters) (add1 sum))]))]))

(define (infer-mem-chars line)
  (cond
    [(string=? "\"\"" line) 6]
    [else 
     (let loop [(head (second (string->list line)))
                (characters (drop (string->list line) 2))
                (sum 3)]
       (cond
         [(and (eq? #\" head) (empty? characters)) (+ 3 sum)]
         [(not (eq? #\\ head))
          (loop (first characters) (rest characters) (add1 sum))]
         [(or (eq? #\\ (first characters)) (eq? #\" (first characters)))
          (loop (second characters) (rest (rest characters)) (+ sum 4))]
         [else
          (loop (first characters) (rest characters) (+ 2 sum))]))]))


; test inputs
(display "get tests\n")
(get-mem-chars "\"\"")
(get-mem-chars "\"abc\"")
(get-mem-chars "\"\\x27\"")
(get-mem-chars "\"aaa\\\"aaa\"")
(get-mem-chars "\"\\\\\"")

(display "infer tests\n")
(infer-mem-chars "\"\"")
(infer-mem-chars "\"abc\"")
(infer-mem-chars "\"\\x27\"")
(infer-mem-chars "\"aaa\\\"aaa\"")
(infer-mem-chars "\"\\\\\"")

;; part 1 solution
  
(with-input-from-file "advent-2015-8.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (+ (get-num-chars line) (- (get-mem-chars line))))))

;; part 2 solution

(with-input-from-file "advent-2015-8.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (+ (infer-mem-chars line) (- (get-num-chars line))))))