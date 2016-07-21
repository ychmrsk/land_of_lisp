;; symbol
(eq 'fooo 'FoOo)

;; number
(+ 1 1.0)  ; 2.0
(expt 53 53)  ; 2435....
(/ 4 6)  ; 2/3
(/ 4.0 6)  ; 0.6666667

;; string
(princ "Tutti Frutti")
(princ "He yelled \"Stop that thief\" from the busy street.")


;; code-mode and data-mode
;; code-mode is dafault mode
;; (command arg1 arg2 ...)
(expt 2 3)
(expt 2 (+ 3 4))

;; data-mode
'(expt 2 3)


;; cons
(cons 'chicken 'cat)  ; (CHICKEN . CAT)
(cons 'chicken 'nil)  ; (CHICKEN)
(cons 'chicken ())  ; (CHICKEN)
(cons 'pork '(beef chicken))  ; (PORK BEEF CHICKEN)
(cons 'beef (cons 'chicken ()))  ; (BEEF CHICKEN)
(cons 'pork (cons 'beef (cons 'chicken ())))  ; (PORK BEEF CHICKEN)

;; car cdr
(car '(pork beef chicken))  ; PORK
(cdr '(pork beef chicken))  ; (BEEF CHICKEN)
(car (cdr '(pork beef chicken)))  ; BEEF
(cadr '(pork beef chicken))  ; BEEF

;; list
(cons 'pork (cons 'beef (cons 'chicken ())))
(list 'pork 'beef 'chicken)
'(pork beef chicken)

(car '((peas carrots tomatoes) (pork beef chicken)))  ; (PEAS CARROTS TOMATOES)
(cdr '(peas carrots tomatoes))  ; (CARROTS TOMATOES)
(cdr (car '((peas carrots tomatoes) (pork beef chicken))))  ; (CARROTS TOMATOES)
(cdar '((peas carrots tomatoes) (pork beef chicken)))  ; (CARROTS TOMATOES)

(cddr '((peas carrots tomatoes) (pork beef chicken) duck))  ; (DUCK)
(caddr '((peas carrots tomatoes) (pork beef chicken) duck))  ; DUCK
(cddar '((peas carrots tomatoes) (pork beef chicken) duck))  ; (TOMATOES)
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck))  ; BEEF

