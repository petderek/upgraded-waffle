#lang racket

(struct position (horiz depth aim))

(struct command (direction magnitude))

(define commands
  (with-input-from-file "2.txt"
    (lambda ()
      (for/list [(line (in-lines))]
        (let [(data (string-split line))]
          (command
           (first data)
           (string->number (second data))))))))
           
             
(define (apply-cmd-v1 pos cmd)
  (match (command-direction cmd)
    ["forward"   (struct-copy
                  position
                  pos
                  [horiz (+ (command-magnitude cmd) (position-horiz pos))])]
    ["down"   (struct-copy
               position
               pos
               [depth (+ (command-magnitude cmd) (position-depth pos))])]
    ["up"   (struct-copy
             position
             pos
             [depth (+ (- (command-magnitude cmd)) (position-depth pos))])]))

(define (apply-cmd-v2 pos cmd)
  (match (command-direction cmd)
    ["forward"   (struct-copy
                  position
                  pos
                  [horiz (+ (command-magnitude cmd) (position-horiz pos))]
                  [depth (+ (* (command-magnitude cmd) (position-aim pos)) (position-depth pos))])]
    ["down"   (struct-copy
               position
               pos
               [aim (+ (command-magnitude cmd) (position-aim pos))])]
    ["up"   (struct-copy
             position
             pos
             [aim (+ (- (command-magnitude cmd)) (position-aim pos))])]))

(define (run-with applier)
  (let loop [(cmd (first commands))
             (remaining (rest commands))
             (pos (position 0 0 0))]
    (let [(next (applier pos cmd))]
      (cond
        [(empty? remaining) (* (position-depth next) (position-horiz next))]
        [else (loop (first remaining) (rest remaining) next)]))))

;; part 1

(run-with apply-cmd-v1)

;; part 2

(run-with apply-cmd-v2)
