(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))


(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let ((foo (+ 2 3)))
  (* foo foo))

(let1 foo (+ 2 3)
      (* foo foo))

(macroexpand '(let1 foo (+ 2 3)
               (* foo foo)))
; (LET ((FOO (+ 2 3))) (* FOO FOO))


(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(split '(2 3)
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
; This can be split into 2 and (3).

(split '()
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
; This cannot be split.


(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
               (f tail (1+ acc))
               acc)))
    (f lst 0)))

(split (progn (princ "Lisp rocks!")
              '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
; Lisp rocks!Lisp rocks!Lisp rocks!This can be split into 2 and (3).

(macroexpand '(split (progn (princ "Lisp rocks!")
                           '(2 3))
                    (format t "This can be split into ~a and ~a." head tail)
                    (format t "This cannot be split.")))

; (IF (PROGN (PRINC "Lisp rocks!") '(2 3))
;  (LET
;   ((HEAD (CAR (PROGN (PRINC "Lisp rocks!") '(2 3))))
;    (TAIL (CDR (PROGN (PRINC "Lisp rocks!") '(2 3)))))
;   (FORMAT T "This can be split into ~a and ~a." HEAD TAIL))
;  (FORMAT T "This cannot be split."))


;; before
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

;; after
(defmacro split (val yes no)
  `(let1 x ,val
         (if x
             (let ((head (car x))
                   (tail (cdr x)))
               ,yes)
             ,no)))

(macroexpand '(split (progn (princ "Lisp rocks!")
                           '(2 3))
                    (format t "This can be split into ~a and ~a." head tail)
                    (format t "This cannot be split.")))

; (LET ((X (PROGN (PRINC "Lisp rocks!") '(2 3))))
;  (IF X
;   (LET ((HEAD (CAR X)) (TAIL (CDR X)))
;    (FORMAT T "This can be split into ~a and ~a." HEAD TAIL))
;   (FORMAT T "This cannot be split.")))

(split (progn (princ "Lisp rocks!")
              '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
; Lisp rocks!This can be split into 2 and (3).


(let1 x 100
      (split '(2 3)
             (+ x head)
             nil))
; *** - +: (2 3) is not a number

(macroexpand '(split '(2 3)
               (+ x head)
               nil))
; (LET ((X '(2 3))) (IF X (LET ((HEAD (CAR X)) (TAIL (CDR X))) (+ X HEAD)) NIL))

;; X = 100  ==>  X = '(2 3)


;; before
(defmacro split (val yes no)
  `(let1 x ,val
         (if x
             (let ((head (car x))
                   (tail (cdr x)))
               ,yes)
             ,no)))

;; after
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))

(macroexpand '(split '(2 3)
               (+ x head)
               nil))
; (LET ((#:G3457 '(2 3)))
;   (IF #:G3457 (LET ((HEAD (CAR #:G3457)) (TAIL (CDR #:G3457))) (+ X HEAD)) NIL))

; (LET ((#:G3458 '(2 3)))
;  (IF #:G3458 (LET ((HEAD (CAR #:G3458)) (TAIL (CDR #:G3458))) (+ X HEAD)) NIL))



(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(pairs '(a b c d e f))
; ((A . B) (C . D) (E . F))
(pairs '(a b c d e))
; ((A . B) (C . D))


(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p)
                    ,@body))
           (self ,@(mapcar #'cdr p)))))


(recurse (n 9)
         (fresh-line)
         (if (zerop n)
             (princ "lift-off!")
             (progn (princ n)
                    (self (1- n)))))
; 9
; 8
; 7
; 6
; 5
; 4
; 3
; 2
; 1
; lift-off!


;; version 1
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

;; version 2
(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

;; version 3
(defun my-length (lst)
  (recurse (lst lst acc 0)
           (split lst
                  (self tail (1+ acc))
                  acc)))


;; for reference
(recurse (n 9)
         (fresh-line)
         (if (zerop n)
             (princ "lift-off!")
             (progn (princ n)
                    (self (1- n)))))


;; functional programming
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))

