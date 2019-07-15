;;
;;
;;

(define undefined
  (lambda l 'undefined))

(define > undefined)
(define <= undefined)
(define >= undefined)
(define abs undefined)
(define not undefined)
(define and undefined)
(define or undefined)
(define cadr undefined)
(define caddr undefined)
(define caaddr undefined)
(define null? undefined)
(define length undefined)
(define reverse undefined)
(define append undefined)
(define appends undefined)
(define map undefined)
(define filter undefined)
(define fold-left undefined)
(define fold-right undefined)
(define compose undefined)
(define fact undefined)
(define fib undefined)
(define sort undefined)
(define eval undefined)

(load "lib.lisp")

;(display (command-line)) (display " ") (display (length (command-line)))
;(newline)

(define (test2 name fct args result)
  (cond ((eq? (length (command-line)) 1)
         (test name fct args result))
        ((equal? (cadr (command-line)) "info") (display fct) (display " : ") (display args) (newline))
        ((equal? (cadr (command-line)) name)
         (test name fct args result))))

(define (test2-comp fct lst arg result)
  (cond ((eq? (length (command-line)) 1)
         (test-comp fct lst arg result))
        ((equal? (cadr (command-line)) "info") (display " compose ") (display " : ") (display arg) (newline))
        ((equal? (cadr (command-line)) "compose")
         (test-comp fct lst arg result))))

(define (test name fct args result)
  (let ((r (apply fct args)))
    (cond ((equal? r result) (display name) (display " : OK"))
          ((eq? r 'undefined) (display name) (display " : UNDEFINED"))
          (#t (display name) (display " : KO ") (display args) (display " = ")
              (display r) (display " != ") (display result)))
    (newline)
    ))

(define (test-comp fct lst arg result)
  (let* ((name "compose")
         (r1 (apply fct lst))
         (r (apply r1 arg)))
    (cond ((equal? r result) (display name) (display " : OK"))
          ((eq? r 'undefined) (display name) (display " : UNDEFINED"))
          (#t (display name) (display " : KO ")
              (display arg) (display " = ")
              (display r) (display " != ") (display result)))
    (newline)
    ))


;; TESTS

(test2 ">" > '(2 0) #t)
(test2 ">" > '(2 2) #f)

(test2 ">=" >= '(2 0) #t)
(test2 ">=" >= '(2 2) #t)
(test2 ">=" >= '(0 2) #f)

(test2 "<=" <= '(2 0) #f)
(test2 "<=" <= '(2 2) #t)
(test2 "<=" <= '(0 2) #t)

(test2 "abs" abs '(-21) 21)
(test2 "abs" abs '(21) 21)

(test2 "not" not '(#f) #t)
(test2 "not" not '(#t) #f)

(test2 "or" or '(#f #t) #t)
(test2 "or" or '(#f #f) #f)
(test2 "or" or '(#t #t) #t)

(test2 "and" and '(#f #t) #f)
(test2 "and" and '(#t #t) #t)
(test2 "and" and '(#f #f) #f)

(test2 "cadr" cadr '((1 2 3)) 2)
(test2 "caddr" caddr '((1 2 3)) 3)
(test2 "caaddr" caaddr '((1 2 (3 4))) 3)

(test2 "null?" null? '(()) #t)
(test2 "null?" null? '((1 2)) #f)

(test2 "length" length '(()) 0)
(test2 "length" length '((1 2 3 4)) 4)

(test2 "reverse" reverse '((1 2 3 4)) '(4 3 2 1))
(test2 "reverse" reverse '((toto titi tata)) '(tata titi toto))

(test2 "append" append '((1 2 3) (4 5 6)) '(1 2 3 4 5 6))
(test2 "append" append '((1 2 3) ()) '(1 2 3))
(test2 "append" append '(() (4 5 6)) '(4 5 6))

(test2 "appends" appends '(((1 2) (3 4) (5 6))) '(1 2 3 4 5 6))
(test2 "appends" appends '(((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
      '(1 2 3 4 5 6 7 8 9 10 11 12))

(test2 "map" map (list (lambda (x) (* x 2)) '(1 2 3 4 5)) '(2 4 6 8 10))
(test2 "map" map (list (lambda (x) (cons x (* x 2))) '(1 2 3 4 5))
      '((1 . 2) (2 . 4) (3 . 6) (4 . 8) (5 . 10)))

(test2 "filter" filter (list (lambda (x) (< x 0)) '(1 -1 2 -2 -3 -4 3 4 -5))
      '(-1 -2 -3 -4 -5))

(test2 "filter" filter (list null? '(() (1 2 3) () (4 5) ()))
      '(() () ()))

(test2 "fold-left" fold-left (list * 1 '(1 2 3 4 5)) 120)
(test2 "fold-left" fold-left (list (lambda (acc x) (cons x acc)) '()
                                  '(1 2 3 4)) '(4 3 2 1))

(test2 "fold-right" fold-right (list * 1 '(1 2 3 4 5)) 120)
(test2 "fold-right" fold-right (list (lambda (x acc) (cons acc x)) '()
                                   '(1 2 3 4))
      '((((() . 4) . 3) . 2) . 1))

(test2-comp compose (list (list (lambda (x) (* x 2)) car)) '((21 42)) 42)
(test2-comp compose (list (list (lambda (x) (* x 2))
                               (lambda (x) (+ x 1)))) '(20) 42)

(test2 "fact" fact (list 5) 120)
(test2 "fact" fact (list 10) 3628800)

(test2 "fib" fib (list 10) 55)
(test2 "fib" fib (list 30) 832040)

(test2 "sort" sort '((5 10 2 7 6 8 4 3 9 1)) '(1 2 3 4 5 6 7 8 9 10))
(test2 "sort" sort '((999 888 777 666 555 444 333 222 111 000))
      '(000 111 222 333 444 555 666 777 888 999))
