#lang racket

;; Exercise 1.1 [m] Define a version of last-name that handles "Rex Morgan MD,"
;; "Morton Downey, Jr.," and whatever other cases you can think of.
(define (last-name name)
  (if (member (last name) *name-suffixes*)
      (last-name (all-but-last name))
      (last name)))

(define *name-suffixes*
  '(Sr. Jr. PhD MD))

(define (all-but-last list)
  (reverse (rest (reverse list))))

;; Exercise 1.2 [m] Write a function to exponentiate, or raise a number to an
;; integer power. For example: (power 3 2) = 3^2 = 9.
(define (power x n)
  (cond ((zero? n) 1)
        (else (* x (power x (- n 1))))))

;; Exercise 1.3 [m] Write a function that counts the number of atoms in an
;; expression. For example: (count-atoms '(a (b) c)) = 3. Notice that there is
;; something of an ambiguity in this: should (a nil c) count as three atoms,
;; or as two, because it is equivalent to (a () c)?
(define (count-atoms expr)
  (length (flatten expr)))

;; Exercise 1.4 [m] Write a function that counts the number of times an expression
;; occurs anywhere within another expression. Example:
;; (count-anywhere 'a '(a ((a) b) a)) => 3.
(define (count-anywhere sub expr)
  (length (filter (lambda (e) (eq? sub e)) (flatten expr))))

;; Exercise 1.5 [m] Write a function to compute the dot product of two sequences
;; of numbers, represented as lists. The dot product is computed by multiplying
;; corresponding elements and then adding up the resulting products. Example:
;; (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
(define (dot-product as bs)
  (apply + (map (lambda (a b) (* a b)) as bs)))
  