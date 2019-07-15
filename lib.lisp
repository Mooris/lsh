(define >
  (lambda (a b)
    (cond ((< b a) #t)
          (#t #f))))

(define >=
  (lambda (a b)
    (cond ((< b a) #t)
          ((eq? a b) #t)
          (#t #f))))

(define <=
  (lambda (a b)
    (cond ((< b a) #f)
          (#t #t))))

(define not
  (lambda (e)
    (cond (e #f)
          (#t #t))))

(define and
  (lambda (a b)
    (cond ((not a) #f)
          ((not b) #f)
          (#t #t))))

(define or
  (lambda (a b)
    (cond (a #t)
          (b #t)
          (#t #f))))

(define abs
  (lambda (nbr)
    (cond ((< nbr 0) (- nbr))
          (#t nbr))))

(define cadr
  (lambda (l)
    (car (cdr l))))

(define caddr
  (lambda (l)
    (car (cdr (cdr l)))))

(define caaddr
  (lambda (l)
    (car (car (cdr (cdr l))))))


(define null?
  (lambda (e) (eq? e '())))

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (#t (+ 1 (length (cdr l)))))))

(define rev_
  (lambda (l res)
    (cond ((null? l) res)
          (#t (rev_ (cdr l) (cons (car l) res))))))

(define reverse
  (lambda (l)
    (rev_ l '())))

(define append
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          (#t (cons (car l1) (append (cdr l1) l2))))))

(define appends
  (lambda (lsts)
    (cond ((null? lsts) '())
          (#t (append (car lsts)
                      (appends (cdr lsts)))))))

(define map
  (lambda (fct lst)
    (cond ((null? lst) '())
          (#t (cons (fct (car lst)) (map fct (cdr lst)))))))

(define filter
  (lambda (fct lst)
    (cond ((null? lst) '())
          ((fct (car lst)) (cons (car lst) (filter fct (cdr lst))))
          (#t (filter fct (cdr lst))))))

(define fold-left
  (lambda (fct acc lst)
    (cond ((null? lst) acc)
          (#t (fold-left fct (fct acc (car lst)) (cdr lst))))))

(define fold-right
  (lambda (fct end lst)
    (cond ((null? lst) end)
          (#t (fct (car lst) (fold-right fct end (cdr lst)))))))


(define compose_
  (lambda (fcts v)
    (cond
     ((null? fcts) v)
     (#t (compose_ (cdr fcts) ((car fcts) v))))))

(define compose
  (lambda (lst)
    (lambda (val)
      (compose_ (reverse lst) val))))

(define fact
  (lambda (n)
    (cond ((< n 1) 1)
          (#t (* n (fact (- n 1)))))))

(define fib
  (lambda (n)
    (cond ((< n 2) n)
          (#t (+ (fib (- n 1)) (fib (- n 2)))))))

(define add-in-tree
  (lambda (tree elem)
    (cond
     ((null? tree) (list elem '() '()))
     (#t (cond
         ((< elem (car tree)) (list (car tree)
                                    (add-in-tree (car (cdr tree)) elem)
                                    (car (cdr (cdr tree)))))
         (#t (list (car tree)
                  (car (cdr tree))
                  (add-in-tree (car (cdr (cdr tree))) elem))))))))

(define tree-to-list
  (lambda (tree)
    (cond
     ((null? tree) '())
     (#t (appends (list (tree-to-list (car (cdr tree)))
                        (cons (car tree) '())
                        (tree-to-list (car (cdr (cdr tree))))))))))

(define sort
  (lambda (lst)
    (tree-to-list (fold-left add-in-tree '() lst))))
