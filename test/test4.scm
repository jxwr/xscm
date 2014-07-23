
(set 'a 100)

(cond ((> a 100) 'gt-100)
      ((= a 100) 'eq-100)
      (t 'other))

(if (= (% a 2) 0) 'even 'odd)

(quote a)

(set 'list (lambda args args))

(list 1 2 3)

(quote (1 2 3 4))

(car (list 5 4 3 2 1 2 3 4))
(cdr (list 5 4 3 2 1 2 3 4))

(car (list))
(= (cdr (list 1)) nil)
(= (car (cdr (list 1 2 3))) 2)

(set 'lst nil)
(cons 1 nil)
(cons 1 (list 1 2 3))












;; According to Richard P. Gabriel in his paper on the subject, the two are defined as follows

;; Lisp-1 has a single namespace that serves a dual role as the function namespace and value namespace; that is, its function namespace and value namespace are not distinct. In Lisp-1, the functional position of a form and the argument positions of forms are evaluated according to the same rules.

;; Lisp-2 has distinct function and value namespaces. In Lisp-2, the rules for evaluation in the functional position of a form are distinct from those for evaluation in the argument positions of the form. Common Lisp is a Lisp-2 dialect.

