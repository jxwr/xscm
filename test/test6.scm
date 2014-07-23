
;(1 2 . 3)
(set 'test-dot (lambda (a b . c) (+ a b (car c))))

(test-dot 10 20 30 40)

; prelude

(set 'list (lambda args args))

(set 'setq (macro (name val)
		  (list 'set (list 'quote name) val)))

(setq f-body (lambda (e)
	       (cond ((atom e) e)
		     ((= (cdr e) ()) (car e))
		     (t (cons 'progn e)))))

(f-body (+ 2 100))

(setq defmacro (macro (name args . body)
		      (list 'setq name (list 'macro args (f-body body)))))

(defmacro defun (name args . body)
  (list 'setq name (list 'lambda args (f-body body))))

(defun add2 (x) (+ 2 x))

(add2 200)

(defmacro define (name . body)
  (if (atom name)
      (list 'setq name (car body))
    (cons 'defun (cons (car name) (cons (cdr name) body)))))

(define a 100)

(define (caar x) (car (car x)))
(caar (list (list 1 2 3) 4))

(defun hello (a b c) 
  (+ a b c))

(defun identity (x) x)
;(defun consp (x) (not (atom x)))

(defun map (fn lst)
  (if (atom lst) lst
    (cons (fn (car lst)) (map fn (cdr lst)))))

(map identity 100)

(cons (identity (car (list 1)))
      (identity (cdr (list 1 2))))

(map (lambda (x) x) (1 . 2))

;(set 'iter (lambda (x f) (f x)))


;(define (x) (+ x 1))

;(setq f-body f-body-func)
;(cons 1 ())
;; (set 'a 100)
;; function
; (progn
; (set 'list (lambda args args)) 
; (set 'add3 (lambda (a b c) (+ a b c)))
; (add3 1 2 3)
; (set 'add (lambda (n . m) (list n m)))
; (add 1 2 3 4)
; (atom 1)
; (atom (1 . 2))
; (cons 1 2))

;(set 'test-macro (macro (name val  others)
;		   (list name val others)))

;(test-macro + 1 2)