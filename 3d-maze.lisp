;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 3D Maze
;;;;

(defconstant +maze-screen-size+ 280)
(defconstant +left-margin+ 180)
(defparameter *top-left* '(180 0))
(defparameter *top-right* '(460 0))
(defparameter *bottom-left* '(180 280))
(defparameter *bottom-right* '(460 280))
(defparameter *maze-screen-rect*
  (sdl:rectangle :x (car *top-left*)
		 :y (cadr *top-left*)
		 :w +maze-screen-size+
		 :h +maze-screen-size+))

(defparameter x-list '(15 60 90 112 128))

(defparameter x-width-list
  (flet ((r-width (x)
	   (- +maze-screen-size+ (* 2 x))))
    (loop for i in x-list collect (list i (r-width i)))))

(defparameter rect-list
  (loop for i  in x-width-list
	collect
	(sdl:rectangle :x (+ (car i) +left-margin+)
		       :y (car i)
		       :w (cadr i)
	             :h (cadr i))))


(defstruct tile
  north
  west)

(defstruct field
  init-pos
  maze)

(defstruct vision-struct
  front
  left
  right
  xy-diff)

(defparameter *vision-data* (make-hash-table))

;;;usage (cdr (assoc 'left (gethash :north vision-data))) -> :west
;;;usage (cdr (assoc 'xy-diff (gethash :north VISION-DATA)))
;;;->"((-1 -3) (0 -3) (1 -3) (-1 -2) (0 -2) (1 -2) (-1 -1) (0 -1) (1 -1) (-1 0) (0 0)(1 0))"

(defun vision-right (front)
  (cdr (assoc 'right (gethash front *vision-data*))))
(defun vision-left (front)
  (cdr (assoc 'left (gethash front *vision-data*))))
(defun vision-diff (front)
  (cdr (assoc 'xy-diff (gethash front *vision-data*))))

(setf (gethash :north *vision-data*) '((front . :north) (left . :west) (right . :east)
				       (xy-diff . ((-1 -3 nil) (0 -3 nil) (1 -3 t)
						   (-1 -2 nil) (0 -2 nil) (1 -2 t)
						   (-1 -1 nil) (0 -1 nil) (1 -1 t)
						   (-1  0 nil) (0  0 nil) (1  0 t)))))
(setf (gethash :south *vision-data*) '((front . :south) (left . :east) (right . :west)
				       (xy-diff . ((1  3 nil) (0  3 nil) (-1  3 t)
						   (1  2 nil) (0  2 nil) (-1  2 t)
						   (1  1 nil) (0  1 nil) (-1  1 t)
						   (1  0 nil) (0  0 nil) (-1  0 t)))))
(setf (gethash :west *vision-data*) '((front . :west) (left . :south) (right . :north)
				      (xy-diff . ((-3 1 nil) (-3 0 nil) (-3 -1 t)
						  (-2 1 nil) (-2 0 nil) (-2 -1 t)
						  (-1 1 nil) (-1 0 nil) (-1 -1 t)
						  (0  1 nil) (0  0 nil) (0  -1) t))))
(setf (gethash :east *vision-data*) '((front . :east) (left . :north) (right . :south)
				      (xy-diff . ((3 -1 nil) (3 0 nil) (3 1 t)
						  (2 -1 nil) (2 0 nil) (2 1 t)
						  (1 -1 nil) (1 0 nil) (1 1 t)
						  (0 -1 nil) (0 0 nil) (0 1 t)))))

(defparameter *vision-temp* '((nil nil nil)
			      (nil nil nil nil)
			      (nil nil nil)
			      (nil nil nil nil)
			      (nil nil nil)
			      (nil nil nil nil)
			      (nil nil nil)
			      (nil nil nil nil)))

(defparameter *vision-rank* '(10 7 4 1))

(defun diffed-dir (x y dir i mz)
  (let* ((v (list (list nil nil nil) nil nil))
	 (diff (vision-diff dir))
	 (xx (modify-x (+ x (car (nth i diff)))))
	 (yy (modify-y (+ y (cadr (nth i diff)))))
	 (f  (get-dir xx yy dir mz))
	 (l  (get-dir xx yy (vision-left dir) mz))
	 (r  (get-dir xx yy (vision-right dir) mz)))
    (setf (second (car v)) f)
    (setf (first  (car v)) l)
    (setf (third  (car v)) r)
    (when (eq l :path)
      (setf (second v)
	    (get-dir (modify-x (+ x (car (nth (1- i) diff))))
		     (modify-y (+ y (cadr (nth (1- i) diff))))
		     dir mz)))
    (when (eq r :path)
      (setf (third v)
	    (get-dir (modify-x (+ x (car (nth (1+ i) diff))))
		     (modify-y (+ y (cadr (nth (1+ i) diff))))
		     dir mz)))
    v))

(defun get-vision (x y dir mz)
  (let ((result nil)
	(temp nil)
	(v-rank '(10 7 4 1)))
    (dolist (v v-rank)
      (format t "v= ~a~%" v)
      (format t "~a~%" (diffed-dir x y dir v mz))
      (setf temp (diffed-dir x y dir v mz))
      (setf result (cons temp result))
      (if (eq (second (car temp)) :path)
	  (continue)
	  (return result)))))

(defun 1st-line (v)
  (let ((s "+"))
    (if (null (second v))
	(setf s (concatenate 'string s "   +"))
	(setf s
	      (concatenate 'string
			   s
			   (h-keyword-str (second v))
			   "+")))
    (setf s (concatenate 'string
			 s
			 (h-keyword-str (second (car v)))
			 "+"))
    (if (null (third v))
	(setf s (concatenate 'string s "   +"))
	(setf s (concatenate 'string
			     s
			     (h-keyword-str (third v))
			     "+")))
    s))
(defun 2nd-line (v)
  (let ((s " "))
    (if (null (second v))
	(setf s (concatenate 'string s "   "))
	(setf s (concatenate 'string s " . ")))
    (setf s (concatenate 'string
			 s
			 (v-keyword-str (first (car v)))))
    (setf s (concatenate 'string
			 s
			 " . "
			 (v-keyword-str (third (car v)))))
    (when (third v)
      (setf s (concatenate 'string s " . ")))
    s))

(defun draw-vision (vision)
  (dolist (v vision)
    (format t "~a~%" (1st-line v))
    (format t "~a~%" (2nd-line v))))

(defun get-vision-* (x y dir mz) ;; 旧版
  (let ((xx 0)
	(yy 0)
	(h1 nil)
	(v1 nil)
	(h2 nil)
	(v2 nil)
	(left  (vision-left dir))
	(right (vision-right dir)))
    (dolist (i (vision-diff dir))
      (setf xx (modify-x (+ x (car i))))
      (setf yy (modify-y (+ y (cadr i))))
      (setf h1  (cons (get-dir xx yy dir  mz) h1))
      (setf v1  (cons (get-dir xx yy left mz) v1))
      (when (caddr i)
	(setf v1 (cons (get-dir xx yy right mz) v1))
	(setf h2 (cons (reverse h1) h2))
	(setf v2 (cons (reverse v1) v2))
	(setf h1 nil)
	(setf v1 nil)))
      (format t "h2 = ~a~%" h2)
      (format t "v2 = ~a~%" v2)
      (values (reverse h2) (reverse v2)))) ;最後に、横と縦のリストを多値で返す。

(defun hv-to-str (hv-list)
  (if (= (length hv-list) 3)
      (let ((str "+"))
	(dolist (i hv-list)
	  (setf str (concatenate 'string str (h-keyword-str i) "+")))
	str)
      (let ((str (v-keyword-str (car hv-list))))
	(dolist (i (cdr hv-list))
	  (setf str (concatenate 'string str " . " (v-keyword-str i))))
	str)))

;; (defun draw-vision-* (x y dir mz)
;;   (let ((h-str "+")
;; 	(v-str nil))
;;     (multiple-value-bind (h v)
;; 	(get-vision-* x y dir mz)
;;       (dolist (j v)
;; 	(dolist (i h)
;; 	  (setf h-str (concatenate 'string
;; 				   h-str
;; 				   (h-keyword-str i) "+")))
;; 	(format t "~a~%" h-str)
;; 	(setf h-str "+")
      
    
  
;;   ())
;;;;
;;;;
;;;;

(defun set-up-field ()
  (make-field
   :init-pos '(0 (1- +map-height+))
   :maze (aops:generate (lambda () (make-tile :north :path :west :path))
			+map-size+)))

;;;;

(defun index->1d (x y)
  (+ (* y +map-height+) x))

(defun index->2d (i)
  (reverse
   (multiple-value-list
    (floor i +map-width+))))

(defun modifiy-xy (x y)
 (let ((xx x)
       (yy y))
   (and (or (< xx 0) (> xx +map-width+))
	(setf xx (mod xx +map-width+)))
   (and (or (< yy 0) (> yy +map-height+))
	(setf yy (mod yy +map-height+)))
   (list xx yy)))

(defun modify-x (x)
  (let ((xx x))
    (and (or (< xx 0) (>= xx +map-width+))
	 (setf xx (mod xx +map-width+)))
    xx))

(defun modify-y (y)
  (let ((yy y))
    (and (or (< yy 0) (>= yy +map-width+))
	 (setf yy (mod yy +map-width+)))
    yy))
;;;;
;;;;
;;;;

(defun get-tile (x y mz)
  (aref mz (index->1d x y)))

(defun get-dir (x y d mz)
  (let ((xx (modify-x x))
	(yy (modify-y y)))
    (cond ((eq d :east)
	   (setf xx (modify-x (1+ xx))))
	  ((eq d :south)
	   (setf yy (modify-y (1+ yy)))))
    (let ((tl (get-tile xx yy mz)))
      (cond ((or (eq d :west)
		 (eq d :east))
	     (tile-west tl))
	    ((or (eq d :north)
		 (eq d :south))
	     (tile-north tl))))))

(defun set-tile (x y tl mz)
  (setf (aref mz (index->1d x y)) tl))

(defun set-dir-1d (i dir wall mz)
  (let ((x (car (index->2d i)))
	(y (cadr (index->2d i))))
    (set-dir x y dir wall mz)))

(defun set-dir (x y dir wall mz)
  (let ((xx x)
	(yy y))
    (cond ((eq dir :east)
	   (setf xx (modify-x (1+ xx))))
	  ((eq dir :south)
	   (setf yy (modify-y (1+ yy)))))
    (cond ((or (eq dir :west)
	       (eq dir :east))
	   (setf (tile-west (aref mz (index->1d xx yy))) wall))
	  ((or (eq dir :north)
	       (eq dir :south))
	   (setf (tile-north (aref mz (index->1d xx yy))) wall)))))

(defun get-around (x y mz)
  (let ((result nil))
    (dolist (i *dir-list*)
      (cons (get-dir x y i mz) result))
    (reverse result)))

;;;;
;;;;
;;;;

(defun dump-maze  (mz)
  (loop for y below +map-height+ do
	(loop for x below +map-width+ do
	      (format t "~a: N:~a E:~a W:~a S:~a~%"
		      (index->1d x y)
		      (get-dir x y :north mz)
		      (get-dir x y :east  mz)
		      (get-dir x y :west  mz)
		      (get-dir x y :south mz)))))

(defun h-keyword-str (k)
  (cond ((eq k :wall) '"---")
	((eq k :path) '"   ")
	((eq k :door) '"-#-")
	(t '"???")))

(defun v-keyword-str (k)
  (cond ((eq k :wall) '"|")
	((eq k :path) '" ")
	((eq k :door) '"#")
	(t '"?")))

(defun draw-maze (mz)
  (let ((1st-line "")
	(2nd-line ""))
    (loop for y below +map-height+ do
      (loop for x below +map-width+ do
	(setf 1st-line (concatenate 'string 1st-line
				    '"+"
				    (h-keyword-str (get-dir x y :north mz))))
	(setf 2nd-line (concatenate 'string 2nd-line
				    (v-keyword-str (get-dir x y :west mz))
				    '" . ")))
      (setf 2nd-line (concatenate 'string 2nd-line
				  (v-keyword-str (get-dir +map-width+ y
							  :west
							  mz))))
      (format t "~a+~%" 1st-line)
      (setf 1st-line "")
      (format t "~a~%" 2nd-line)
      (setf 2nd-line ""))
    (loop for x below +map-width+ do
      (format t "+~a" (h-keyword-str (get-dir x 0 :north mz))))
    (format t "+~%")))

;;;;
;;;;
;;;;

(defun set-maze (mz wd)
  (let ((n nil))
    (loop for i below +map-size+ do
      (setf n (nth i wd))
      (set-dir-1d i :north (car n) mz)
      (set-dir-1d i :west  (cadr n) mz))))

(defun init-dungeon ()
  (setf *dungeon* (set-up-field))
  (set-maze (field-maze *dungeon*) *wall-data*))
