#lang racket
;;Define a macro multiple-apply, (multiple-apply (fun-1 fun-2 ...) to list-1 list-2 ...) where
;;fun-i are functions and to is a keyword, which returns a list containing the result of applying fun-i to
;list-i.


(define-syntax multiple-apply
  (syntax-rules (to)
    ((_ (f1 ...) to l1 ...)       ;;repeat operators and lists
     (list (apply f1 l1) ...))))  ;;repeat application of the operator to the list


(multiple-apply (+ - * /) to '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3))



(define (producer ag1 ag2)
  (let loop ((i 1))
    (if (< i 10)
        (begin
          ((if (odd? i) ag1 ag2) i)
          (loop (+ i 1)))
        (cons
         (ag2 "end")
         (ag1 "end")))))