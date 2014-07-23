(set 'list (lambda args args))

(set 'mt (lambda (a b . c)
	  (- (+ a b) (car c))))

(mt 1 3 4 5 6 7)

(set 'test-m (macro (a b . c)
		    c))

(set 'mm (macro (a b . c)
		(list '- (list '+ a b) (car c))))

(mm 1 2 3 4 5 6)
(test-m 1 2 (+ 1 2))

