#lang typed/racket

(: uchicago151-version : Integer)
(define uchicago151-version 1)
(provide uchicago151-version)

(: uchicago151-date : String)
(define uchicago151-date "September 29, 2015")
(provide uchicago151-date)

(define-syntax uchicago151-define-struct
  (syntax-rules ()
    [(uchicago151-define-struct (name A ...) (fields ...))
     (define-struct (A ...) name (fields ...) #:transparent)]
    [(uchicago151-define-struct name (fields ...))
     (define-struct name (fields ...) #:transparent)]))
(provide (rename-out [uchicago151-define-struct define-struct]))
;; NOTE Make sure the CS tutors know about this change to define-struct

(: uchicago151-cons : (All (A) (-> A (Listof A) (Listof A))))
(define (uchicago151-cons hd tl) (cons hd tl))
(provide (rename-out [uchicago151-cons cons]))

(: uchicago151-map : (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (uchicago151-map f xs) (map f xs))
(provide (rename-out [uchicago151-map map]))

(: uchicago151-filter : (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (uchicago151-filter f xs) (filter f xs))
(provide (rename-out [uchicago151-filter filter]))

(: uchicago151-foldl : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (uchicago151-foldl f acc xs) (foldl f acc xs))
(provide (rename-out [uchicago151-foldl foldl]))

(: uchicago151-foldr : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (uchicago151-foldr f acc xs) (foldr f acc xs))
(provide (rename-out [uchicago151-foldr foldr]))

(: uchicago151-partition :
   (All (A) (-> (-> A Boolean) (Listof A) (values (Listof A) (Listof A)))))
(define (uchicago151-partition f xs) (partition f xs))
(provide (rename-out [uchicago151-partition partition]))

(: uchicago151-andmap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (uchicago151-andmap f xs) (andmap f xs))
(provide (rename-out [uchicago151-andmap andmap]))

(: uchicago151-ormap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (uchicago151-ormap f xs) (ormap f xs))
(provide (rename-out [uchicago151-ormap ormap]))

(: forbidden-function (-> String String))
(define (forbidden-function f)
  (string-append "uchicago151: " f
                 ": You may not use the built-in function " f
                 " in this course; you must write your own such function."))

(: uchicago151-argmax : (All (A) (-> (-> A Real) (Listof A) A)))
(define (uchicago151-argmax f xs)
  (error (forbidden-function "argmax")))
(provide (rename-out [uchicago151-argmax argmax]))

(: uchicago151-argmin : (All (A) (-> (-> A Real) (Listof A) A)))
(define (uchicago151-argmin f xs)
  (error (forbidden-function "argmin")))
(provide (rename-out [uchicago151-argmin argmin]))

(: real-sqrt : Real -> Real)
(define (real-sqrt x)
  (if (not (negative? x))
      (sqrt x)
      (error (string-append "real-sqrt expects a nonnegative real; given "
                            (number->string x)))))
(provide real-sqrt)
