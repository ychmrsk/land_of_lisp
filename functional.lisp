;; 関数型プログラミング

;; + 参照透過性 
;; + 外部の変数を参照しない（const参照はOK）
;; + 変数の値を変更しない
;; + 関数の目的は、引数から値を計算すること
;; + 外の世界に影響を与えない
;; + 外の世界から情報を受け取らない

;; 副作用 = 命令的 procedual


;; functional
(defun add-widget (database widget)
  (cons widget database))

;; procedual
(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~a~%" *database*)))




(defparameter *my-list* '(4 7 2 3))

(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))))

(add-two '(4 7 2 3))

(mapcar (lambda (x)
          (+ x 2))
        '(4 7 2 3))




