;; chapter 11 : format

(format t "Add onion rings for only ~$ dollars more!" 1.5)
; Add onion rings for only 1.50 dollars more!

(format OUTPUT "STRING_SEQUENCE_INCLUDE_~$" value)
;       nil  : string
;       t    : console
;       stream

(format t "I am printing ~s in the middle of this sentence." "foo")
; I am printing "foo" in the middle of this sentence.
(format t "I am printing ~a in the middle of this sentence." "foo")
; I am printing foo in the middle of this sentence.

(format t "I am printing ~10a within ten spaces of room." "foo")
; I am printing foo        within ten spaces of room.
(format t "I am printing ~10@a within ten spaces of room." "foo")
; I am printing        foo within ten spaces of room.
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
; I am printing foo          within ten (or more) spaces of room.

(format t "I am printing ~,,4a in the middle of this sentence." "foo")
; I am printing foo     in the middle of this sentence

(format t "The word ~,,4,'!a feels very important." "foo")
; The word foo!!!! feels very important.

(format t "The word ~,,4,'!@a feels very important." "foo")
; The word !!!!foo feels very important.

;; ------------------------------------------------------------
(format t "The number 1000 in hexadecimal is ~x" 1000)
; The number 1000 in hexadecimal is 3E8

(format t "The number 1000 in binary is ~b" 1000)
; The number 1000 in binary is 1111101000

(format t "The number 1000 in decimal is ~d" 1000)
; The number 1000 in decimal is 1000

(format t "Numbers with commas in them are ~:d times better." 1000000)
; Numbers with commas in them are 1,000,000 times better.

(format t "I am printing ~10d within ten spaces of room" 1000000)
; I am printing    1000000 within ten spaces of room

(format t "I am printing ~10,'xd within ten space of room" 1000000)
; I am printing xxx1000000 within ten space of room


;; ------------------------------------------------------------
(format t "PI can be estimated as ~4f" 3.141593)
; PI can be estimated as 3.14

(format t "PI can be estimated as ~,4f" 3.141593)
; PI can be estimated as 3.1416

(format t "PI can be estimated as ~,4f" pi)
; PI can be estimated as 3.1416

(format t "Percentages are ~,,2f percent better than fractions" 0.77)
; Percentages are 77.0 percent better than fractions

(format t "I wish I had ~$ dolalrs in my bank account." 1000000.2)
; I wish I had 1000000.20 dolalrs in my bank account.


;; ------------------------------------------------------------
;; fresh-line : newline if need
;; terpri     : newline

(progn (princ 22)
       (terpri)
       (princ 33))

(progn (princ 22)
       (fresh-line)
       (princ 33))

(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))

(progn (princ 22)
       (terpri)
       (terpri)
       (princ 33))

;; combination with format
;; ~% : newline
;; ~& : newline if need

(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))

(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

(format t "this will print ~5%on two lines spread far apart")

;; ------------------------------------------------------------

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(random-animal)

(loop repeat 10
   do (format t "~5t~a ~15t~a ~25t~a~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(loop repeat 10
   do (format t "~30<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

;;  ~30< : 文字揃え開始 
;;  ~a   : 値を表示する 
;;  ~;   : 新たに揃える値を始める
;;  ~a   : 値を表示する 
;;  ~;   : 新たに揃える値を始める
;;  ~a   : 値を表示する 
;;  ~>   : 文字揃え終了
;;  ~%   : 改行文字の出力

(loop repeat 10
   do (format t "~30:@<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(loop repeat 10
   do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))


(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a!~%~}" *animals*)
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))


