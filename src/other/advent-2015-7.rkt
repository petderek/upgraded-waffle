#lang racket

(define MAX_INTEGER_SIZE 65535)

(define (flip char)
  (match char
    [#\1 #\0]
    [#\0 #\1]))



(define (string->func str)
  (match str
    ["AND" bitwise-and]
    ["NOT" (lambda (x y) (bitwise-not x))]
    ["OR"  bitwise-ior]
    ["LSHIFT" arithmetic-shift]
    ["RSHIFT" (lambda (x y) (arithmetic-shift x (- y)))]
    [_ (lambda (x y) (identity x))]))

;; a wire is just a command, two inputs, and an output
;; special case for not/identity
(struct wire
  (command x y out))

(define (line->wire line)
  (let* [(split (string-split line " -> "))
         (line-out (last split))
         (line-in (first split))
         (pieces (string-split line-in " "))]
    ;; couple of cases here -- the line-in can have 1-3 terms.
    ;; 1 term -> direct numeral input
    ;; 2 terms -> NOT function
    ;; 3 terms -> standard function with two inputs
    (match (length pieces)
      [1 (wire (string->func "IDENTITY") (first pieces) (first pieces) line-out)]
      [2 (wire (string->func "NOT") (last pieces) (last pieces) line-out)]
      [3 (wire (string->func (second pieces)) (first pieces) (last pieces) line-out)])))
    
 

(define (get-wires)
  (with-input-from-file "advent-2015-7.txt"
    (lambda ()
      (for/list ([line (in-lines)])
        (line->wire line)))))


(define (get-arg ht val)
  (if (string->number val)
      (string->number val)
      (hash-ref ht val #f)))

(define (normalize x)
  (if (negative? x)
      (+ MAX_INTEGER_SIZE x 1)
      x))

;; solution 1

(let loop [(ht (make-hash))
           (wires (get-wires))]
  (let* [(wire (first wires))
         (first-arg (get-arg ht (wire-x wire)))
         (second-arg (get-arg ht (wire-y wire)))
         (has-args (and first-arg second-arg))
         (func (wire-command wire))]
    (cond
      [has-args
       (hash-set!
        ht
        (wire-out wire)
        (normalize (func first-arg second-arg)))])

    (cond
      [(empty? (rest wires)) (hash-ref ht "a")]
      [has-args (loop ht (rest wires))]
      [else (loop ht (append (rest wires) (list wire)))])))

;; solution 2


(let loop [(ht (make-hash))
           (wires (get-wires))]
  (let* [(wire (first wires))
         (first-arg (get-arg ht (wire-x wire)))
         (second-arg (get-arg ht (wire-y wire)))
         (has-args (and first-arg second-arg))
         (func (wire-command wire))]
    (cond
      [(eq? "b" (wire-out wire))
       (hash-set!
        ht
        "b"
        3176)]
      [has-args
       (hash-set!
        ht
        (wire-out wire)
        (normalize (func first-arg second-arg)))])

    (cond
      [(empty? (rest wires)) (hash-ref ht "a")]
      [has-args (loop ht (rest wires))]
      [else (loop ht (append (rest wires) (list wire)))])))