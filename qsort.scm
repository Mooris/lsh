(define (null? l) (eq? l '()))

;;;;;

(define (merge-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2)))
        (#t                    (cons (car l2) (merge-lists l1 (cdr l2))))))

(define (split-half l l1 l2)
  (cond ((null? l) (cons l1 l2))
        ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2))
        (#t (split-half (cdr (cdr l))
                        (cons (car l) l1)
                        (cons (car (cdr l)) l2)))))

(define (merge-sort lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (#t (let ((lsts (split-half lst '() '())))
              (merge-lists (merge-sort (car lsts))
                           (merge-sort (cdr lsts)))))))


;;;;;

(define (append l1 l2)
  (cond ((null? l1) l2)
        (#t (cons (car l1) (append (cdr l1) l2)))))

(define (sup-inf pivot lst inf eq sup)
  (cond ((null? lst) (list inf eq sup))
        (#t (let ((first (car lst)) (rest (cdr lst)))
              (cond ((< first pivot)   (sup-inf pivot rest
						(cons first inf) eq sup))
		    ((eq? first pivot) (sup-inf pivot rest
						inf (cons first eq) sup))
                    (#t                (sup-inf pivot rest
						inf eq (cons first sup))))))))

(define (qsort lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (#t (let ((pivot (car lst)) (lst2 (cdr lst)))
              (let ((lsts (sup-inf pivot lst2 '() '() '())))
                (append (qsort (car lsts))
			(append (cons pivot (car (cdr lsts)))
				(qsort (car (cdr (cdr lsts)))))))))))

(define (length l)
  (cond ((null? l) 0)
        (#t (+ 1 (length (cdr l))))))



