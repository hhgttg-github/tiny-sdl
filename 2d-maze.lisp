;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 2D Maze
;;;;

(defvar +2d-maze-width+ 8) ; mazeは正方形 5x5
(defvar +2d-maze-size+ (* +2d-maze-width+
			  +2d-maze-width+))

(defstruct tile
  (sprite 0)
  (type   :path)
  (looked nil)
  (enterd nil)
  (event  0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-1d (x y)
  (+ x (* +2d-maze-width+ y)))

(defun to-2d (i)
  (reverse
   (multiple-value-list
    (floor i +2d-maze-width+))))

(defun to-screen (i)
  (mapcar #'1+ (to-2d i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun over-map-edge (e)
  (let ((result e))
    (or (and (< result 0)
	     (setf result (1- +2d-maze-width+)))
	(and (>= result +2d-maze-width+)
	     (setf result 0)))
    result))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tiles* (iterate (for i below +2d-maze-size+)
			(collect (make-tile))))

(defun set-tile (i tile)
  (setf (nth i *tiles*) tile))

(defun get-tile (i)
  (nth i *tiles*))

(defun set-type (i type)
  (setf (tile-type (nth i *tiles*)) type))

(defun get-type (i)
  (tile-type (nth i *tiles*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-edge ()
  (let ((tmp nil)
	(tile nil))
    (loop for i below +2d-maze-size+ do
	  (setf tmp (to-2d i))
	  (and (or (= (car tmp) 0)
		   (= (car tmp) (1- +2d-maze-width+))
		   (= (cadr tmp) 0)
		   (= (cadr tmp) (1- +2d-maze-width+)))
	       (setf tile (get-tile i))
	       (setf (tile-sprite tile) 2)
	       (setf (tile-type tile) :wall)
	       (set-tile i tile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-tile (i)
  (let ((tile (get-tile i))
	(xy (to-screen i)))
    (draw-cell *sp-sheet*
	       (car xy)
	       (cadr xy)
	       (tile-sprite tile))))

(defun draw-3x3 (i)
  (let* ((xy (to-2d i))
	 (x (car xy))
	 (y (cadr xy)))
    (iterate
     (for dy from -1 to 1)
     (iterate
      (for dx from -1 to 1)
      (draw-tile (to-1d (over-map-edge (+ dx x))
			(over-map-edge (+ dy y))))))))

(defun draw-all-map ()
  (iterate (for i below +2d-maze-size+)
	   (draw-tile i)))

(defun draw-2d-map ()
  (let ((tmp nil))
    (loop for i below +2d-maze-size+ do
      (setf tmp (get-tile i))
      (cond ((= (tile-sprite tmp) 0)
	     (format t "~a" (c-str "  " :bg-white)))
	    ((= (tile-sprite tmp) 1)
	     (format t "~a" (c-str "  " :bg-black)))
	    (t
	     (format t "~a" (c-str "  " :bg-red))))
      (when (= (car (to-2d i)) (1- +2d-maze-width+))
	(format t "~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; First Procedures
;;;;

(defun read-map ()
  (set-edge ))

(defun init-map () ;;; テスト用
  (read-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; テスト
;;;;

(defun map-test ()
  (init-map)
  (sdl:with-init ()
    (sdl:window +screen-width+ +screen-height+
		:title-caption "マップ テスト" ) 
    (setf (sdl:frame-rate) 30)
    (init-bg "Background.png")
    (init-sp "Sprite.png" 16)
    
    (sdl:draw-surface-at-* *bg* 0 0)

    (draw-all-map)
    
    (sdl:update-display)
    
    (sdl:with-events ()
      (:quit-event() t)
      (:key-down-event (:key key)
		       (when (sdl:key= key :sdl-key-escape )
			 (sdl:push-quit-event)))
      (:idle ()

	     (sdl:update-display )))))


