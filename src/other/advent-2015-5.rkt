#lang racket


(define (extract-vowels str)
  (for/fold
   ([acc ""])
   ([c (in-string str)])
    (if (string-contains? "aeiou" (string c))
        (~a acc c)
        acc)))

(define (extract-repeaters str)
  (regexp-match* #px"([a-z])\\1" str))

(define (has-three-vowels str)
  (let ([vowels (extract-vowels str)])
    (<= 3
        (string-length vowels))))

(define (has-one-double-letter str)
  (let ([doubles (extract-repeaters str)])
    (<= 1
        (length doubles))))

(define (no-naughty-pairs str)
  (let [(naughty-pairs (list "ab" "cd" "pq" "xy"))]
    (not
     (for/or [(naughty-pair naughty-pairs)]
       (string-contains? str naughty-pair)))))

(define (contains-repeaters str)
  (not
   (empty?
    (regexp-match* #px"([a-z])([a-z]).*\\1\\2" str))))

(define (contains-sandwiches str)
  (not
   (empty?
    (regexp-match* #px"([a-z])[a-z]\\1" str))))

(define (is-nice str)
  (and
   (has-three-vowels str)
   (has-one-double-letter str)
   (no-naughty-pairs str)))

(define (is-nice-v2 str)
  (and
   (contains-repeaters str)
   (contains-sandwiches str)))

;; testing inputs

(is-nice "ugknbfddgicrmopn")
(is-nice "aaa")
(is-nice "haegwjzuvuyypxyu")
(is-nice "jchzalrnumimnmhp")
(is-nice "dvszwmarrgswjxmb")

(is-nice-v2 "qjhvhtzxzqqjkmpb")
(is-nice-v2 "xxyxx")
(is-nice-v2 "uurcxstgmygtbstg")
(is-nice-v2 "ieodomkazucvgmuy")

;; part 1 solution -- count nice strings

(with-input-from-file "advent-2015-5.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (if (is-nice line)
          1
          0))))

;; part 2 solution -- count nice strings v2

(with-input-from-file "advent-2015-5.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (if (is-nice-v2 line)
          1
          0))))
