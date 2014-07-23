(list 1 2 3)

(defun macroexpand (e)
  ((label mexpand
	  (lambda (e env f)
            (progn
	     (while (and (consp e)
			 (not (member (car e) env))
			 (set 'f (macrocallp e)))
		    (set 'e (macroapply f (cdr e))))
	     (if (and (consp e)
		      (not (or (eq (car e) 'quote)
			       (eq (car e)  quote))))
		 (let ((newenv
			(if (and (or (eq (car e) 'lambda) (eq (car e) 'macro))
				 (consp (cdr e)))
			    (append.2 (cadr e) env)
			    env)))
		   (map (lambda (x) (mexpand x newenv nil)) e))
		 e))))
   e nil nil))

(list 1 2 3)

(setq sp '| |)
(setq nl '|a
b|)

(setq defmacro
      (macro (name args body)
	(list 'setq name (list 'macro args (f-body body)))))
(set a 100)

(lambda (x) (+ 1 x))
