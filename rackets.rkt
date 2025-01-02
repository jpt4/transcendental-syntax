;;  rackets.rkt
;;  20250101Z
;;  jpt4
;;  Executable implementation of Stellar Resolution in Racket.
;;  Racket v8.14

#lang racket

(require minikanren)
(require minikanren/numbers)
; (require alphakanren) ;;TODO: package alphakanren for Racket.

#|
Binary increment in stellogen:

inc = +inc($atom($0) $atom($1)); 
      +inc($atom($1) $atom($0:$1)); 
      +inc($atom($0:X) $atom($1:X)); 
      +inc($atom($1:X) $atom($0:Y)) -inc($atom(X:$0) $atom(Y)).

Per Alea:
print process
  inc.
  -inc($atom($1:$1) R) $res(R).
  clean.
end

;; prints 6 copies of the result

res(atom(0:0:1:0));
res(atom(0:0:1:0));
res(atom(0:0:1:0));
res(atom(0:0:1:0));
res(atom(0:0:1:0));
res(atom(0:0:1:0));
|#

;;  Binary increment in miniKanren
(define (inco i o)
  (fresh (x y res)
         (conde
          [(== '(0) i) (== '(1) o)]
          [(== '(1) i) (== '(0 1) o)]
          [(== `(0 . ,x) i) (== `(1 . ,x) o)]
          [(== `(1 . ,x) i) (== `(0 . ,y) o) (inco x y)]
          )))

#|
racket@rackets> (run 1 (q) (inco '(0) '(1)))
'(_.0)
racket@rackets> (run 1 (q) (inco '(1) '(0 1)))
'(_.0)
racket@rackets> (run 1 (q) (inco '(1 1) '(0 0 1)))
'(_.0)
racket@rackets> (run 1 (q) (inco '(1 1) q))
'((0 0 1))
racket@rackets> (run 1 (q) (inco '(1 1 0) q))
'((0 0 1))
racket@rackets> (run 1 (q) (inco '(1 1 0 1) q))
'((0 0 1 1))
racket@rackets> (run 1 (q) (inco '(1 0 1 0 1 1 0 1) q))
'((0 1 1 0 1 1 0 1))
|#
