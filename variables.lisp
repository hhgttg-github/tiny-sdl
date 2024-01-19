
(defparameter *opening*
  (make-instance 'states
		 :name "opening"
		 :message "start"
		 :prompt "start-quit"))

(defparameter *global-states* *opening*)

(defparameter *quit-game* nil)
