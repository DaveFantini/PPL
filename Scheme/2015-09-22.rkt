#lang racket
;;Consider a list (x0x1 . . . xn). Its sublist from i to j is the list (xi, xi+1 . . . xj ). Define the procedure co-sublist
;;which, given a list L and two indexes i and j, i ≤ j, returns the list of ordered elements of L that are not in
;;the sublist from i to j. You cannot use procedures with side effects in your code (e.g. set!).

(define (co-sublist L i j)
  (let ((lst '()))
    (when (< j i)
        L)
    (when (<= i j)
        (for-each (lambda (arg)

                    (when (or (< (index-of L arg) i) (> (index-of L arg) j))
                        (set! lst (append lst (list arg)))))L)
      lst)))