#lang racket

(define instructions
  (with-input-from-file "2.txt"
    (lambda () (port->lines))))

(define (horizfunc line)
  (let* [(data (string-split line " "))
         (direction (first data))
         (magnitude (string->number (second data)))]
    (lambda (horiz)
      (match direction
        ["forward" (+ horiz magnitude)]
        [else horiz]))))

(define (depthfunc line)
  (let* [(data (string-split line " "))
         (direction (first data))
         (magnitude (string->number (second data)))]
    (lambda (depth)
      (match direction
        ["down" (+ depth magnitude)]
        ["up"   (+ depth (- magnitude))]
        [else depth]))))


(define (aim-depth-func line aim)
  (let* [(data (string-split line " "))
         (direction (first data))
         (magnitude (string->number (second data)))]
    (lambda (depth)
      (match direction
        ["forward" (+ depth (* aim magnitude))]
        [else depth]))))

;; part 1

(let loop [(current (first instructions))
           (upcoming (rest instructions))
           (horiz 0)
           (depth 0)]
  (let [(next-depth ((depthfunc current) depth))
        (next-horiz     ((horizfunc current) horiz))]
    (cond
      [(empty? upcoming) (* next-depth next-horiz)]
      [else (loop (first upcoming) (rest upcoming) next-horiz next-depth)])))


;; part 2

(let loop [(current (first instructions))
           (upcoming (rest instructions))
           (horiz 0)
           (depth 0)
           (aim 0)]
  (let [(next-horiz ((horizfunc current) horiz))
        (next-depth ((aim-depth-func current aim) depth))
        (next-aim ((depthfunc current) aim))]
    (cond
      [(empty? upcoming) (* next-depth next-horiz)]
      [else (loop (first upcoming) (rest upcoming) next-horiz next-depth next-aim)])))