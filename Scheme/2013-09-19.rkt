#lang racket

;;Define an object, using the “closures as objects” technique seen in class, that works as a simple immutable
;;container of integer numbers. It must offer two methods: member?, that checks if a number is contained in
;;the object; and subsetsum, that checks if a given number is the sum of elements contained in the object (at
;;most each element must be taken once).


(define (object l)
  (let ((list l))

    ;;Methods
    (define (member? x)
      (if (list? (member x list))
          #t
          #f))
    
    (define (subsetsum y)
      (let ((trovato #f))
        (for-each (lambda (arg)
                    (for-each (lambda (arg2)
                                (when (eqv? (+ arg arg2) y)
                                    (set! trovato #t))) list)) list)
        trovato))

    ;;Dispatcher
  (lambda (msg . args)
    (apply (case msg
             ((member) member?)
             ((subsetsum) subsetsum))
           args))))
                           
                                    