;; association list (alist)
;; used only symbol and list, not string
(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

;; (assoc target from)
;; ex: (assoc 'garden *nodes*)
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; debug
;; (print (describe-location 'living-room *nodes*))
;; (print (describe-location 'garden *nodes*))
;; (print (describe-location 'attic *nodes*))


(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; debug
;; (print (describe-path '(garden west door)))

;; #' : function operator
;; There are namespace of variables and namespace of functions respectively.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; debug
;; (print (apply #'append (mapcar #'describe-path (cdr (assoc 'living-room *edges*)))))
;; (print (apply #'append (mapcar #'describe-path (cdr (assoc 'garden *edges*)))))
;; (print (apply #'append (mapcar #'describe-path (cdr (assoc 'attic *edges*)))))

;; ;; (assoc location edges)
;; (LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)) 
;; (GARDEN (LIVING-ROOM EAST DOOR)) 
;; (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER)) 
;; ;; (cdr (assoc location edges))
;; ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)) 
;; ((LIVING-ROOM EAST DOOR)) 
;; ((LIVING-ROOM DOWNSTAIRS LADDER)) 
;; ;; (mapcar #'describe-path (cdr (assoc location edges)))
;; ((THERE IS A DOOR GOING WEST FROM HERE.) (THERE IS A LADDER GOING UPSTAIRS FROM HERE.)) 
;; ((THERE IS A DOOR GOING EAST FROM HERE.)) 
;; ((THERE IS A LADDER GOING DOWNSTAIRS FROM HERE.)) 
;; ;; (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))
;; (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.) 
;; (THERE IS A DOOR GOING EAST FROM HERE.) 
;; (THERE IS A LADDER GOING DOWNSTAIRS FROM HERE.) 


(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)  ; predicate
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; (assoc obj obj-locs)  ; (whiskey living-room)
;; (cadr (assoc obj obj-locs))  ; living-room
;; (eq (cadr (assoc obj obj-locs)) loc)  ; T of NIL

;; debug
;; (print (objects-at 'living-room *objects* *object-locations*))
;; (print (objects-at 'garden *objects* *object-locations*))
;; (print (objects-at 'attic *objects* *object-locations*))


(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; debug
;; (print (describe-objects 'living-room *objects* *object-locations*))
;; (print (describe-objects 'garden *objects* *object-locations*))
;; (print (describe-objects 'attic *objects* *object-locations*))


(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; debug
;; (print (look))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))


;; debug
;; (print (cdr (assoc *location* *edges*)))
;; (print (find 'west (cdr (assoc *location* *edges*))
;;              :key #'cadr))
;; (print (find 'upstairs (cdr (assoc *location* *edges*))
;;              :key #'cadr))
;; (print (find 'east (cdr (assoc *location* *edges*))
;;              :key #'cadr))
;;  
;; (setf *tmp* (find 'west (cdr (assoc *location* *edges*))
;;                   :key #'cadr))
;; (print (car *tmp*))


(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carring the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

