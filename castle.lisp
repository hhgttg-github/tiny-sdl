
(defparameter *global-stage* "opening")
(defparameter *local-stage* "start/quit")

(defclass stage ()
  ((message-key :accessor message-key :initarg :message-key)
   (prompt-key  :accessor prompt-key  :initarg :prompt-key)))

(defparameter *message-table* (make-hash-table :test #'equal))
(defparameter *prompt-table* (make-hash-table :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *opening* 
  (make-instance 'stage
		 :message-key "opening"
		 :prompt-key "start/quit"))

(defparameter *quit-y/n* 
  (make-instance 'stage
		 :message-key ""
		 :prompt-key "quit-y/n"))

(defparameter *start/quit*
  (make-instance 'stage
		 :message-key ""
		 :prompt-key "start/quit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (gethash "opening" *message-table*) "TINY GAME")

(setf (gethash "start/quit" *prompt-table*) '("S)tart Q)uit"))

(setf (gethash "quit-y/n" *prompt-table*) '("ゲームを終了しますか?" "Y)es N)o"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun redraw-screen *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-stage (key)
  (switch (*global-stage* :test #'equal)
    ("opening" (process-opening key))))

(defun process-opening (key)

  )

	       
