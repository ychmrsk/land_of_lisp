;; LISP is Symmetry

(if '()
    'i-am-true
    'i-am-false)

(if '(1)
    'i-am-true
    'i-am-false)


(defun my-length (list)
  (if list
      (1+ (my-length (cdr lst)))
      0))

(my-length '(list with four symbols))  ; 4


;; '()  ()  'nil  nil

(eq '() nil)  ; T
(eq '() ())   ; T
(eq '() 'nil) ; T

;; ============================================================
;; if
;; ============================================================
;; ifの持つ二つの式のうち、どちらか一方だけが実際に評価される
;; if文でできることは一つだけ

(if (= (+ 1 2) 3)
    'yup
    'nope)

(if (= (+ 1 2) 4)
    'yup
    'nope)

(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(if (oddp 5)
    'odd-number
    'even-number)

(if (oddp 5)
    'odd-number
    (/ 1 0))


(defvar *number-was-odd* nil)  ; *NUMBER-WAS-ODD*

(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number)  ; ODD-NUMBER

*number-was-odd*  ; T


;; when and unless

(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)
*number-is-odd*  ; T

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)
*number-is-odd*  ; NIL


;; cond

(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
                            '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
                             '(i hope you chocked on my pudding johnny))
        (t                   '(why you eat my pudding stranger?))))

(pudding-eater 'henry)
(pudding-eater 'johnny)
(pudding-eater 'george-clooney)


;; case

(defun pudding-eater (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
             '(curse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
              '(i hope you chocked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger?))))


;; and / or

(and (oddp 5) (oddp 7) (oddp 9))  ; T
(or (oddp 4) (oddp 7) (oddp 8))  ; T

(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t))
*is-it-even*  ; T


(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))
(and *file-modified (ask-user-about-saving) (save-file))
(if (and *file-modified*
         (ask-user-about-saving))
    (save-file))


;; others

(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list)

(member 1 '(3 4 1 5))  ; (1 5)

(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list)

(member nil '(3 4 nil 5))  ; (nil 5) , not nil


(find-if #'oddp '(2 4 5 6))
(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number)

(find-if #'null '(2 4 nil 6))  ; NIL , oops...


;; eq, equal, eql, =, string-equal, equalp
;; 比較に関するコンラッドのルール
;; 1. シンボル同士は常にEQで比較すべし
;; 2. シンボル同士の比較でなければEQUALをつかえ

(defparameter *fruit* 'apple)

(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))  ; symbol vs symbol


(equal 'apple 'apple)  ; T

(equal (list 1 2 3) (list 1 2 3))  ; T

(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))  ; T

(equal 5 5)  ; T

(equal 2.5 2.5)  ; T

(equal "foo" "foo")  ; T

(equal #\a #\a)  ; T


;; eql : eq + 'number' + 'character'
(eql 'foo 'foo)  ; symbol
(eql 3.4 3.4)  ; number
(eql #\a #\a)  ; character

;; equalp : equal +α
(equalp "Bob Smith" "bob smith")  ; T
(equalp 0 0.0)  ; T

;; = : number
;; string-equal : string
;; char-equal : char


