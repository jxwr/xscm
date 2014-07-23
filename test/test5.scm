
(set 'list (lambda args args))
; macro
(set 'just (macro (a b c) (list a b c)))
;((macro (a b c) (list a b c)) = 1 2)

(just + 1 2)

(set 'setq (macro (name val)
                  (list 'set (list 'quote name) val)))

(setq a 100)

a

(cdr (1 . 2))
(car (1 . 2))
(cdr (1 . nil))
(1 . nil)
(list 1)

;; TODO:
;; [] dot expression
;; [] recursion call
;; [] builtbin symbols