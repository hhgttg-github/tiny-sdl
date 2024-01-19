
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *message-table* (make-hash-table :test #'equal))

(setf (gethash "opening" *message-table*) "Start")

(defparameter *prompt-table* (make-hash-table :test #'equal))

(setf (gethash "start-quit" *prompt-table*) '("S)tart Q)uit"))
(setf (gethash "quit-y/n"   *prompt-table*) '("終了しますか?" "Y)es, Quit.  N)o"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stage ()
  ((message :accessor message :initarg :message)
   (prompt  :accessor prompt  :initarg :prompt )))

(defclass opening (stage)
  ())

(defclass opening-quit-yn (stage)
  ())

(defparameter *opening*
  (make-instance 'opening
		 :message "opening"
		 :prompt  "start-quit"))

(defparameter *opening-quit-yn*
  (make-instance 'opening-quit-yn
		 :message "opening"
		 :prompt  "quit-yn"))

(defgeneric process (state key))

(defmethod process(opening key)
  (switch (key :test #'sdl:key=)
    (:sdl-key-s)
    (:sdl-key-q)))

(defmethod process(opening-quit-yn key)
  (switch (key :test #'sdl:key=)
    (:sdl-key-y)
    (:sdl-key-n)
    (t)))
