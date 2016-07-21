(cons 1 (cons 2 (cons 3 nil)))  ; (1 2 3)

(cons 1 (cons 2 3))  ; (1 2 . 3)

;; dot list
;; nil以外の値でリストが終端されている

'(1 . (2 . (3 . nil)))  ; (1 2 3)


;; pair
(cons 2 3)  ; (2 . 3)
(car (cons 2 3))  ; 2
(cdr (cons 2 3))  ; 3


;; circle list
(setf *print-circle* t)

(defparameter foo (list 1 2 3))  ; foo
(setf (cdddr foo) foo)  ; #1=(1 2 3 . #1#)


;; alist

(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))

(assoc 'lisa *drink-order*)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

(assoc 'lisa *drink-order*)  ; (LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM)


;; visualization of tree data structure

(defparameter *house* '((walls (mortar (cement)
                                       (water)
                                       (sand))
                               (bricks))
                        (windows (glass)
                                 (frame)
                                 (curtains))
                        (roof (shingles)
                              (chimney))))

;; visualization of graph

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                        there is a well in front of you.))
                               (attic (you are in the attic.
                                       there is a giant torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))


;; dot file

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))



;; nullary function
;; thunk / suspension

(with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello File!" my-stream))


(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))



;; 無向グラフ
(defun uedges->dot (edge)
  (maplist (lambda (lst)
                   (mapc (lambda (edge)
                           (unless (assoc (car edge) (cdr lst))
                             (fresh-line)
                             (princ (dot-name (caar lst)))
                             (princ "--")
                             (princ (dot-name (car edge)))
                             (princ "[label=\"")
                             (princ (dot-label (cdr edge)))
                             (princ "\"];")))
                         (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

