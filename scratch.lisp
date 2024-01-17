(defclass machine ()
  ((name :accessor name :initarg :name)
   (main :accessor main :initarg :main)
   (sub  :accessor sub  :initarg :sub :initform nil)
   (message :accessor message :initarg :messag0e :initform "")
   (prompt  :accessor prompt  :initarg :prompt  :initform nil)
   (table :accessor table :initarg :table :initform nil)))

(defparameter *number-key* '(:sdl-key-0 :sdl-key-1
			     :sdl-key-2 :sdl-key-3
			     :sdl-key-4 :sdl-key-5
			     :sdl-key-6 :sdl-key-7
			     :sdl-key-8 :sdl-key-9))

(defun number-key-p (k)
  (member k *number-key* :test #'sdl:key=))

(defclass machine ()
  ((m :accessor m :initarg :m)
   (p :accessor p :initarg :p)))

(defclass opening (machine)
  ((n :accessor n :initform "opening" :initarg :n)))

(defclass castle (machine)
  ((n :accessor n :initform "castle" :initarg :n)))

(defmethod send-key ((o opening) key)
  (when (sdl:key= key :sdl-key-a)
    (format t "key = A pressed.~%")))

(defmethod send-key ((m machine) key)
  (format t "machine pressed.~%"))

(defparameter *opening*
  (make-instance 'opening
		 :m "opening message"
		 :p "start quit"))

(defparameter *castle*
  (make-instance 'castle
		 :m "castle"
		 :p "store leave"))

(defparameter *active-machine* *opening*)

