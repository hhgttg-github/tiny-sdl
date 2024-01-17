(defparameter +message-TL+ '(18 1))
(defparameter +message-BL+ '(18 12))

(defparameter +message-width+ 21)
(defparameter +message-height+ 12)

(defparameter +prompt-TL+ '(18 14))
(defparameter +prompt-BL+ '(18 16))

(defparameter +prompt-width+ 21)
(defparameter +prompt-height+ 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-prompt-box ()
  (clear-string-box (car +prompt-TL+)
		    (cadr +prompt-TL+)
		    +prompt-width+
		    +prompt-height+))

(defun draw-prompt-line (s n)
  (let* ((l (length-zenkaku s))
	 (margin (ash (- +prompt-width+ l) -1)))
    (draw-string s
		 (+ (car +prompt-TL+) margin)
		 n)))

(defun draw-prompt (s1 &rest s2)
  (if (null s2)
      (draw-prompt-line s1 (1+ (cadr +prompt-TL+)))
      (let ((s (flatten (list s1 s2)))
	    (c (cadr +prompt-TL+)))
	(dolist (i s)
	  (draw-prompt-line i c)
	  (incf c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-message-box ()
  (clear-string-box (car +message-TL+)
		   (cadr +message-TL+)
		   +message-width+
		   +message-height+))

(defun draw-message (str)
  (draw-message-* str (car +message-TL+) (cadr +message-TL+)))

(defun draw-message-* (str x y)
  (let ((l (concatenate 'list str))
	(dx x)
	(dy y)
	(x 0)
	(y 0))

    (loop until (or (alexandria:emptyp l)
		    (= y +message-height+))
	  do
	  (cond
	    ((and (eql (nth 0 l) #\~)
		  (eql (nth 1 l) #\%))
	     (setf x 0)
	     (incf y)
	     (setf l (cddr l)))
	    (t
	     (if (= x +message-width+)
		 (progn
		   (setf x 0)
		   (incf y))
		 (progn
		   (draw-string (string (car l)) (+ x dx) (+ y dy))
		   (setf l (cdr l))
		   (incf x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-box ()
  (draw-message ""))
