;; internals

(set 'list (lambda args args))

(set 'setq (macro (name val)
		  (list 'set (list 'quote name) val)))

(setq f-body (lambda (e)
	       (cond ((atom e) e)
		     ((= (cdr e) ()) (car e))
		     (t (cons 'progn e)))))

(setq defmacro (macro (name args . body)
		      (list 'setq name (list 'macro args (f-body body)))))

(defmacro defun (name args . body)
  (list 'setq name (list 'lambda args (f-body body))))

;(defmacro defvar (name value)
;  '(setq ,name ,value))

(defmacro define (name . body)
  (if (atom name)
      (list 'setq name (car body))
    (cons 'defun (cons (car name) (cons (cdr name) body)))))

(defun map (fn lst)
  (if (atom lst) lst
    (cons (fn (car lst)) (map fn (cdr lst)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

; builtins 
(define (car x) (car x))
(define (cdr x) (cdr x))
(define (cons x y) (cons x y))
(define (> x y) (> x y))
(define (< x y) (< x y))
(define (= x y) (= x y))
(define (% x y) (% x y))
(define (+ x y) (+ x y))
(define (- x y) (- x y))
(define (* x y) (* x y))
(define (set x y) (set x y))
(define (atom x) (atom x))
(define (message x) (message x))
(define (p x) (p x))
(define (progn x) (progn x))
(define (quote x) (quote x))
(define (and x y) (and x y))
(define (not x) (not x))
(define (or x y) (or x y))

(defun identity (x) x)
(setq null not)
(defun consp (x) (not (atom x)))
;(defmacro car (x) (list 'car x))

(defmacro let (binds . body)
  (cons (list 'lambda (map car binds) (f-body body))
        (map cadr binds)))

;; tests

; define variable
(define hundred 100)
(message hundred)

; define function
(define (mul5 x) (* 5 x))
(message mul5 (mul5 9))

; map
(p (map mul5 (list 1 2 3)))

; let
(define (m+n x y) 
  (let ((n x) (m y))
    (+ n m)))
(p (m+n 10 15))

(defun square (a) (* a a))
;(define (square a) (* a a))

(define (fn x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(fn 10 20)

(map (lambda (x) 
       (fn (car x) (cadr x)))
     (list (list 10 20)
	   (list 20 30)
	   (list 30 40)))

'a ; symbol
'1 ; 1
1 ; '1 === 1
'(1 2 3 4)