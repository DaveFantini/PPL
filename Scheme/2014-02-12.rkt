#lang racket
;;Consider a procedure string-from-strings that receives as input a list of objects, keeps all the objects
;;that are strings, discarding all the others, and returns the ordered concatenation of all such strings.


;;Give an implementation of string-from-strings using the classical
;;functional higher order functions, i.e. map, filter, fold...
(define (string-from-strings list)
      (let ((lst (filter string? list)))
      (foldr string-append "" lst)))  

;;Define a functional (non tail) recursive version of string-from-strings
;;(without using map, filter, fold)
(define (string-from-strings-rec list)
  (if (null? list)
      ""
      (if (string? (car list))
          (string-append (car list)(string-from-strings-rec (cdr list)))
          (string-from-strings-rec (cdr list)))))
          
  

;;Define a tail recursive version of string-from-strings
;;(without using map, filter, fold).
(define (string-from-strings-tail list acc)
  (if (null? list)
      acc
      (if (string? (car list))
          (string-from-strings-tail (cdr list) (string-append acc (car list)))
          (string-from-strings-tail (cdr list) acc))))
  