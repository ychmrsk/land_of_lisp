(defun half (n)
  (/ n 2))

#'half

(lambda (n) (/ n 2))

(mapcar (lambda (n) (/ n 2)) '(2 4 6))

;; lambda
;; 名前のない関数
;; 引数は評価されない：通常の関数ではない <マクロ>

;; 値と関数を区別せずに、必要であれば二つの世界をつなぐ

;; lambdaが唯一のlispコマンド？
;; ラムダ算法
