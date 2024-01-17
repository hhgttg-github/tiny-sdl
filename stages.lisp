
(defclass stage ()
  states
  next-stage)

(defparameter *main-stage* nil)
(defparameter *sub-stage* nil)

(defmethod )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *y-n-stage* ()
  (make-instance 'stage
		 :states :wait-y-n-answer
		 :next-stage))

(defparameter *prompt-stage* ()
  (make-instance 'stage
		 :states :wait-prompt
		 :next-stage))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-stage ()
  (setf *stage* *opening*)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun change-stage (st)
  ()
  )
