#lang racket
(define raw-input
  (port->lines (open-input-file "10.txt")))

(define (complement left)
  (match left
    [#\( #\)]
    [#\[ #\]]
    [#\{ #\}]
    [#\< #\>]))

(define (closes? left right)
  (eq? right (complement left)))

(define (first-illegal-char line)
  (let loop [(stk (list))
             (chars (string->list line))]
    (cond
      [(empty? chars) #f]
      [else
       (match (string (first chars))
         [(regexp #px"[\\(\\[\\{\\<]") (loop (append (take chars 1) stk) (rest chars))]
         [(regexp #px"[\\)\\]\\}\\>]")
          (if (closes? (first stk) (first chars))
              (loop (list-tail stk 1) (rest chars))
              (first chars))])])))

(define (get-score c)
  (match c
    [#\) 3]
    [#\] 57]
    [#\} 1197]
    [#\> 25137]))

(define (get-completion-score line)
  (let loop [(chars (string->list line))
             (acc 0)]
    (cond
      [(empty? chars) acc]
      [else
       (define bonus
         (match (first chars)
           [#\) 1]
           [#\] 2]
           [#\} 3]
           [#\> 4]))
       (loop (rest chars) (+ (* acc 5) bonus))])))

(define corrupted-lines
  (filter-not
   first-illegal-char
   raw-input))
   

(define (get-completion line)
  (define stack
    (let loop [(stk (list))
               (chars (string->list line))]
      (cond
        [(empty? chars) stk]
        [else
         (match (string (first chars))
           [(regexp #px"[\\(\\[\\{\\<]") (loop (append (take chars 1) stk) (rest chars))]
           [(regexp #px"[\\)\\]\\}\\>]")
            (if (closes? (first stk) (first chars))
                (loop (list-tail stk 1) (rest chars))
                (first chars))])])))
  (list->string
   (map
    complement
    stack)))

;; problem 1

(for/sum [(line (in-list raw-input))]
  (match (first-illegal-char line)
    [#f 0]
    [c (get-score c)]))

;; problem 2
(define completion-scores
 (for/list [(line (in-list corrupted-lines))]
  (get-completion-score
   (get-completion line))))

(list-ref
 (sort completion-scores <)
 (quotient (length completion-scores) 2))