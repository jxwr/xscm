## xscm

Dirty scheme interpreter, not done yet.

* [ ] support simple macro, no reader macro yet, write macro with pure quote expr.
* [ ] support basic binary operations, > < = % + - * / .
* [ ] no gc!
* [ ] no vm! eval ast directly.
* [ ] quote implementation totally wrong.

## Examples

test/test8.scm

```scheme
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
```

output
```
*MACRO-EXPANSION*:
  (set (quote f-body) (lambda (e) (cond ((atom e) e) ((= (cdr e) ()) (car e)) (t (cons 'progn e)))))
*MACRO-EXPANSION*:
  (set (quote defmacro) (macro (name args . body) (list 'setq name (list 'macro args (f-body body)))))
*MACRO-EXPANSION*:
  (setq defun (macro (name args . body) (list 'setq name (list 'lambda args (f-body body)))))
*MACRO-EXPANSION*:
  (set (quote defun) (macro (name args . body) (list 'setq name (list 'lambda args (f-body body)))))
*MACRO-EXPANSION*:
  (setq define (macro (name . body) (if (atom name) (list 'setq name (car body)) (cons 'defun (cons (car name) (cons (cdr name) body))))))
*MACRO-EXPANSION*:
  (set (quote define) (macro (name . body) (if (atom name) (list 'setq name (car body)) (cons 'defun (cons (car name) (cons (cdr name) body))))))
*MACRO-EXPANSION*:
  (setq map (lambda (fn lst) (if (atom lst) lst (cons (fn (car lst)) (map fn (cdr lst))))))
*MACRO-EXPANSION*:
  (set (quote map) (lambda (fn lst) (if (atom lst) lst (cons (fn (car lst)) (map fn (cdr lst))))))
*MACRO-EXPANSION*:
  (defun caar (x) (car (car x)))
*MACRO-EXPANSION*:
  (setq caar (lambda (x) (car (car x))))
*MACRO-EXPANSION*:
  (set (quote caar) (lambda (x) (car (car x))))
*MACRO-EXPANSION*:
  (defun cadr (x) (car (cdr x)))
*MACRO-EXPANSION*:
  (setq cadr (lambda (x) (car (cdr x))))
*MACRO-EXPANSION*:
  (set (quote cadr) (lambda (x) (car (cdr x))))
*MACRO-EXPANSION*:
  (defun cdar (x) (cdr (car x)))
*MACRO-EXPANSION*:
  (setq cdar (lambda (x) (cdr (car x))))
*MACRO-EXPANSION*:
  (set (quote cdar) (lambda (x) (cdr (car x))))
*MACRO-EXPANSION*:
  (defun cddr (x) (cdr (cdr x)))
*MACRO-EXPANSION*:
  (setq cddr (lambda (x) (cdr (cdr x))))
*MACRO-EXPANSION*:
  (set (quote cddr) (lambda (x) (cdr (cdr x))))
*MACRO-EXPANSION*:
  (defun caaar (x) (car (car (car x))))
*MACRO-EXPANSION*:
  (setq caaar (lambda (x) (car (car (car x)))))
*MACRO-EXPANSION*:
  (set (quote caaar) (lambda (x) (car (car (car x)))))
*MACRO-EXPANSION*:
  (defun caadr (x) (car (car (cdr x))))
*MACRO-EXPANSION*:
  (setq caadr (lambda (x) (car (car (cdr x)))))
*MACRO-EXPANSION*:
  (set (quote caadr) (lambda (x) (car (car (cdr x)))))
*MACRO-EXPANSION*:
  (defun cadar (x) (car (cdr (car x))))
*MACRO-EXPANSION*:
  (setq cadar (lambda (x) (car (cdr (car x)))))
*MACRO-EXPANSION*:
  (set (quote cadar) (lambda (x) (car (cdr (car x)))))
*MACRO-EXPANSION*:
  (defun caddr (x) (car (cdr (cdr x))))
*MACRO-EXPANSION*:
  (setq caddr (lambda (x) (car (cdr (cdr x)))))
*MACRO-EXPANSION*:
  (set (quote caddr) (lambda (x) (car (cdr (cdr x)))))
*MACRO-EXPANSION*:
  (defun cdaar (x) (cdr (car (car x))))
*MACRO-EXPANSION*:
  (setq cdaar (lambda (x) (cdr (car (car x)))))
*MACRO-EXPANSION*:
  (set (quote cdaar) (lambda (x) (cdr (car (car x)))))
*MACRO-EXPANSION*:
  (defun cdadr (x) (cdr (car (cdr x))))
*MACRO-EXPANSION*:
  (setq cdadr (lambda (x) (cdr (car (cdr x)))))
*MACRO-EXPANSION*:
  (set (quote cdadr) (lambda (x) (cdr (car (cdr x)))))
*MACRO-EXPANSION*:
  (defun cddar (x) (cdr (cdr (car x))))
*MACRO-EXPANSION*:
  (setq cddar (lambda (x) (cdr (cdr (car x)))))
*MACRO-EXPANSION*:
  (set (quote cddar) (lambda (x) (cdr (cdr (car x)))))
*MACRO-EXPANSION*:
  (defun cdddr (x) (cdr (cdr (cdr x))))
*MACRO-EXPANSION*:
  (setq cdddr (lambda (x) (cdr (cdr (cdr x)))))
*MACRO-EXPANSION*:
  (set (quote cdddr) (lambda (x) (cdr (cdr (cdr x)))))
*MACRO-EXPANSION*:
  (defun car (x) (car x))
*MACRO-EXPANSION*:
  (setq car (lambda (x) (car x)))
*MACRO-EXPANSION*:
  (set (quote car) (lambda (x) (car x)))
*MACRO-EXPANSION*:
  (defun cdr (x) (cdr x))
*MACRO-EXPANSION*:
  (setq cdr (lambda (x) (cdr x)))
*MACRO-EXPANSION*:
  (set (quote cdr) (lambda (x) (cdr x)))
*MACRO-EXPANSION*:
  (defun cons (x y) (cons x y))
*MACRO-EXPANSION*:
  (setq cons (lambda (x y) (cons x y)))
*MACRO-EXPANSION*:
  (set (quote cons) (lambda (x y) (cons x y)))
*MACRO-EXPANSION*:
  (defun > (x y) (> x y))
*MACRO-EXPANSION*:
  (setq > (lambda (x y) (> x y)))
*MACRO-EXPANSION*:
  (set (quote >) (lambda (x y) (> x y)))
*MACRO-EXPANSION*:
  (defun < (x y) (< x y))
*MACRO-EXPANSION*:
  (setq < (lambda (x y) (< x y)))
*MACRO-EXPANSION*:
  (set (quote <) (lambda (x y) (< x y)))
*MACRO-EXPANSION*:
  (defun = (x y) (= x y))
*MACRO-EXPANSION*:
  (setq = (lambda (x y) (= x y)))
*MACRO-EXPANSION*:
  (set (quote =) (lambda (x y) (= x y)))
*MACRO-EXPANSION*:
  (defun % (x y) (% x y))
*MACRO-EXPANSION*:
  (setq % (lambda (x y) (% x y)))
*MACRO-EXPANSION*:
  (set (quote %) (lambda (x y) (% x y)))
*MACRO-EXPANSION*:
  (defun + (x y) (+ x y))
*MACRO-EXPANSION*:
  (setq + (lambda (x y) (+ x y)))
*MACRO-EXPANSION*:
  (set (quote +) (lambda (x y) (+ x y)))
*MACRO-EXPANSION*:
  (defun - (x y) (- x y))
*MACRO-EXPANSION*:
  (setq - (lambda (x y) (- x y)))
*MACRO-EXPANSION*:
  (set (quote -) (lambda (x y) (- x y)))
*MACRO-EXPANSION*:
  (defun * (x y) (* x y))
*MACRO-EXPANSION*:
  (setq * (lambda (x y) (* x y)))
*MACRO-EXPANSION*:
  (set (quote *) (lambda (x y) (* x y)))
*MACRO-EXPANSION*:
  (defun set (x y) (set x y))
*MACRO-EXPANSION*:
  (setq set (lambda (x y) (set x y)))
*MACRO-EXPANSION*:
  (set (quote set) (lambda (x y) (set x y)))
*MACRO-EXPANSION*:
  (defun atom (x) (atom x))
*MACRO-EXPANSION*:
  (setq atom (lambda (x) (atom x)))
*MACRO-EXPANSION*:
  (set (quote atom) (lambda (x) (atom x)))
*MACRO-EXPANSION*:
  (defun message (x) (message x))
*MACRO-EXPANSION*:
  (setq message (lambda (x) (message x)))
*MACRO-EXPANSION*:
  (set (quote message) (lambda (x) (message x)))
*MACRO-EXPANSION*:
  (defun p (x) (p x))
*MACRO-EXPANSION*:
  (setq p (lambda (x) (p x)))
*MACRO-EXPANSION*:
  (set (quote p) (lambda (x) (p x)))
*MACRO-EXPANSION*:
  (defun progn (x) (progn x))
*MACRO-EXPANSION*:
  (setq progn (lambda (x) (progn x)))
*MACRO-EXPANSION*:
  (set (quote progn) (lambda (x) (progn x)))
*MACRO-EXPANSION*:
  (defun quote (x) (quote x))
*MACRO-EXPANSION*:
  (setq quote (lambda (x) (quote x)))
*MACRO-EXPANSION*:
  (set (quote quote) (lambda (x) (quote x)))
*MACRO-EXPANSION*:
  (defun and (x y) (and x y))
*MACRO-EXPANSION*:
  (setq and (lambda (x y) (and x y)))
*MACRO-EXPANSION*:
  (set (quote and) (lambda (x y) (and x y)))
*MACRO-EXPANSION*:
  (defun not (x) (not x))
*MACRO-EXPANSION*:
  (setq not (lambda (x) (not x)))
*MACRO-EXPANSION*:
  (set (quote not) (lambda (x) (not x)))
*MACRO-EXPANSION*:
  (defun or (x y) (or x y))
*MACRO-EXPANSION*:
  (setq or (lambda (x y) (or x y)))
*MACRO-EXPANSION*:
  (set (quote or) (lambda (x y) (or x y)))
*MACRO-EXPANSION*:
  (setq identity (lambda (x) x))
*MACRO-EXPANSION*:
  (set (quote identity) (lambda (x) x))
*MACRO-EXPANSION*:
  (set (quote null) not)
*MACRO-EXPANSION*:
  (setq consp (lambda (x) (not (atom x))))
*MACRO-EXPANSION*:
  (set (quote consp) (lambda (x) (not (atom x))))
*MACRO-EXPANSION*:
  (setq let (macro (binds . body) (cons (list 'lambda (map car binds) (f-body body)) (map cadr binds))))
*MACRO-EXPANSION*:
  (set (quote let) (macro (binds . body) (cons (list 'lambda (map car binds) (f-body body)) (map cadr binds))))
*MACRO-EXPANSION*:
  (setq hundred 100)
*MACRO-EXPANSION*:
  (set (quote hundred) 100)
100 
*MACRO-EXPANSION*:
  (defun mul5 (x) (* 5 x))
*MACRO-EXPANSION*:
  (setq mul5 (lambda (x) (* 5 x)))
*MACRO-EXPANSION*:
  (set (quote mul5) (lambda (x) (* 5 x)))
#<fn:0x1a32640> 45 
(5 10 15) 
*MACRO-EXPANSION*:
  (defun m+n (x y) (let ((n x) (m y)) (+ n m)))
*MACRO-EXPANSION*:
  (setq m+n (lambda (x y) (let ((n x) (m y)) (+ n m))))
*MACRO-EXPANSION*:
  (set (quote m+n) (lambda (x y) (let ((n x) (m y)) (+ n m))))
*MACRO-EXPANSION*:
  ((lambda (n m) (+ n m)) x y)
25 
*MACRO-EXPANSION*:
  (setq square (lambda (a) (* a a)))
*MACRO-EXPANSION*:
  (set (quote square) (lambda (a) (* a a)))
*MACRO-EXPANSION*:
  (defun fn (x y) (let ((a (+ 1 (* x y))) (b (- 1 y))) (+ (* x (square a)) (* y b) (* a b))))
*MACRO-EXPANSION*:
  (setq fn (lambda (x y) (let ((a (+ 1 (* x y))) (b (- 1 y))) (+ (* x (square a)) (* y b) (* a b)))))
*MACRO-EXPANSION*:
  (set (quote fn) (lambda (x y) (let ((a (+ 1 (* x y))) (b (- 1 y))) (+ (* x (square a)) (* y b) (* a b)))))
*MACRO-EXPANSION*:
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y))
399811
*MACRO-EXPANSION*:
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y))
*MACRO-EXPANSION*:
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y))
*MACRO-EXPANSION*:
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y))
(399811 7205721 43223631)
'a
'1
1
'(1 2 3 4)
```
