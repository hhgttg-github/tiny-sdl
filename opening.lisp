
;; (defparameter *stage* nil) ---- 現在のステージ
;; (defparameter *opening* (make-instance 'opening)) 
;; (setf *stage* *opening*)
;; ()
;;

(defclass state ()
  ((scene :initarg :scene :accessor scene)
   (bind-to :initarg :bind-to :accessor bind-to)))

(defclass scene ()
  ((message :initarg :message :accessor message)
   (bind-to :initarg :bind-to :accessor bind-to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *state-table* (make-hash-table))
;(defparameter *store* (make-hash-table))
(setf (gethash 'store *state-table*) (make-hash-table))
(defparameter *store* (gethash 'store *state-table*)
  
(setf (gethash "退店" *store*)
      (make-instance 'scene
		     :message "店を出ます"
		     :bind-to '((#\press-any-key . 'castle))))

(setf (gethash "商品選択" *store*)
  (make-instance 'scene
		 :message "どれを買いますか?"
		 :bind-to
		 '((#\key-a-z . 3)
		   (#\key-exit . 2))))

(defparameter )

(setf (gethash "store" *state-table*) *store*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *opening*
  (make-instance 'state
		 :scene 'opening
		 :bind-to '((sdl-key-s . 'castle)
			    (sdl-key-q . 'quit-game)
			    (sdl-key-escape . 'quit-game))))

(setf *state-list*
      (cons *opening* *state-list*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stage ()
  live
  scene)

(defparameter y-or-n 
  (make-instance 'stage
		 :live :y-or-n
		 :scene
		 '((sdl-key-y) nil)
		 ((sdl-key-n) nil)))
		 
(defparameter *opening*
  (make-instance 'stage
		 :live :start-or-quit
		 :scene
		 '(:start-or-quit
		   ((sdl-key-s) #'start-game)
		   ((sdl-key-q sdl-key-escape) #'quit-game))))

(defun start-game ()
  (setf *stage* *castle*)
  )

(defun quit-game ()
  (sdl:push-quit-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *castle*
  (make-instance 'stage
		 :current :hall
		 :scene-list
		 '(:hall
		   ((sdl-key-s) #'go-store)
		   ((sdl-key-m) #'go-maze)
		   ((sdl-key-q) #'quit-game))))

(defparameter *store*
  (make-instance 'stage
		 :current :select-item
		 :scene-list
		 '((:select-item
		    ((sdl-key-s) #'select-item))
		   (:leave-store
		    ((sdl-key-y #'go-hall)
		     (t :select-item)))
		   ((sdl-key-p) #'purchase-item)
		   ((sdl-key-c) #'go-hall))))
		   


(defparameter *stage* *opening*)

(defmethod process-stage (opening key)
   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod draw-stage (opening)
  (clear-message-box)
  (let ((s "*** Welcome Message ***"))
    (draw-message-* (center-str s +message-width+)
		    (ash +message-height+ -1)
		    s))
  (clear-prompt-box)
  (let ((p "S)tart Game" "Q)uit Game"))
    (draw-prompt p)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-game ())
(defun quit-game ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-stage (opening key)
  (draw-stage )
  ()
  )
