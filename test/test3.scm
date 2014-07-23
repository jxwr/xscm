
(set 'a 100)
a

(set 'add-2 
     (lambda (x)
       (+ 2 x)))

(add-2 10)

(set 'echo-true-if-2 
     (lambda x 
       (if (= x 2) t f)))

(echo-true-if-2 2)
(echo-true-if-2 3)

(* 10 ((lambda x (+ 1 x)) 100))

((lambda (a b) (+ a b)) 1 2)

(quote a)
(quote (1 2 3 4))

'(a b c d 2 3 4)

; static scope ? 
;(lambda (x) (+ 1 x a))

t

123

'a

(+ 2 4)

(- 5 3)
;(+ (* 1 3) (/ 4 2))

(+ 1 (- 10 5) 30 (/ 6 3))

(* 15 3)

(- 1 2)

(if (> 3 1) 
    (- 1 2) 
    (+ 2 3))

(if (> 13 5) 
    (- (* 2 4) 2) 
    (+ 2 3))

(if (= 2 4) 'yes 'no)

(if t 'yes 'no)

