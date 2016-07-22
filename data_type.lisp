;; array

(make-array 3)  ; #(NIL NIL NIL)

(defparameter x (make-array 3))
(aref x 1)  ; NIL

(setf (aref x 1) 'foo)

x  ; #(NIL FOO NIL)
(aref x 1)  ; FOO


;; generic setter
;; combination of 'aref' and 'set'

(setf foo (list 'a 'b 'c))  ; (A B C)
(second foo)  ; B
(setf (second foo) 'z)  ; Z
foo  ; (A Z C)


(setf foo (make-array 4))  ; #(NIL NIL NIL NIL)
(setf (aref foo 2) (list 'x 'y 'z))  ; (X Y Z)
foo  ; #(NIL NIL (x y z) NIL)
(setf (car (aref foo 2)) (make-hash-table))  ; #S(HASH-TABLE)
(setf (gethash 'zoink (car (aref foo 2))) 5)  ; 5
foo  ; #(NIL NIL (#S(HASH-TABLE (ZOINK . 5)) Y Z) NIL)


;; list vs array

(nth 1 '(foo bar baz))  ; BAR
(aref x 1000)


;; hash table

(make-hash-table)

(defparameter x (make-hash-table))
(gethash 'yup x)  ; NIL ;  <= value of gethash 
                  ; NIL    <= key exists?

(setf (gethash 'yup x) '25)
(gethash 'yup x)  ; 25 ;
                  ; T


(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-latte)

(gethash 'lisa *drink-order*)  ; SMALL-DRIP-COFFEE ;
                               ; T


;; return multi-value

(round 2.4)  ; 2 ;
             ; 0.4000001

(defun foo ()
  (values 3 7))
(foo)  ; 3 ;
       ; 7

(+ (foo) 5)  ; 8

(multiple-value-bind (a b) (foo)
  (* a b))  ; 21


;; performance of hash table
;; => see. wumpus2.lisp


;; structure

(defstruct person
           name
           age
           waist-size
           favorite-color)

(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))
*bob*  ; #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")
(person-age *bob*)
(setf (person-age *bob*) 36)

(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
(person-age *that-guy*)


;; list vs structure
(defun make-person (name age waist-size favorite-color)
  (list name age waist-size favorite-color))

(defun person-age (person)
  (cadr person))

(defparameter *bob* (make-person "bob" 35 32 "blue"))
*bob*
(person-age *bob*)
;; list : immutable
;; structure ; mutable


;; generic data

;; sequence : list array string
(length '(a b c))
(length "blub")
(length (make-array 5))

(find-if #'numberp '(a b 5 d))
(count #\s "mississippi")
(position #\4 "2kewl4skewl")
(some #'numberp '(a b 5 d))
(every #'numberp '(a b 5 d))

;; reduce
(reduce #'+ '(3 4 6 5 2))

(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)

(defun sum (lst)
  (reduce #'+ lst))

(sum '(1 2 3))
(sum (make-array 5 :inital-contents '(1 2 3 4 5)))
(sum "blablabla")  ; error

;; map
(map 'list
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this is a string")

;; subseq, sort
(subseq "america" 2 6)
(sort '(5 8 2 4 9 3 6) #'<)



;; predicate
;; arrayp, characterp, consp, functionp, hash-table-p, listp, stringp, symbolp
(numberp 5)

(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b)) 
        ((and (listp a) (listp b)) (append a b))))
(add 3 4)
(add '(a b) '(c d))

;; type dispatching
;; => defmethod
(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))
(add 3 4)
(add '(a b) '(c d))


