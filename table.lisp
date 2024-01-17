

(defparameter *current-node* "opening")
(defparameter *current-message* nil)
(defparameter *current-prompt* nil)
(defparameter *current-kpj* nil)
(defparameter *next-node* nil)

(defparameter *quit-game* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass prompt ()
  ((ask :accessor ask :initarg :ask)))

(defclass message ()
  ((text :accessor text :initarg :text)))

(defclass node ()
  ((message-key :accessor message-key :initarg :message-key)
   (prompt-key  :accessor prompt-key  :initarg :prompt-key)
   (kpj     :accessor kpj     :initarg :kpj)))     ; ((key process jump)()()..)のリスト

(defparameter *node-table*    (make-hash-table :test #'equal))
(defparameter *message-table* (make-hash-table :test #'equal))
(defparameter *prompt-table*  (make-hash-table :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-tables ()
  
;;;; Node Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setf (gethash "opening" *node-table*)
	(make-instance 'node
		       :message-key "opening"
		       :prompt-key  "Start Quit"
		       :kpj    '((:sdl-key-s nil "castle")
				 (:sdl-key-q nil "quit-game-y-n"))))

  (setf (gethash "quit-game-y-n" *node-table*)
	(make-instance 'node
		       :message-key ""
		       :prompt-key "ゲームを終了しますか?"
		       :kpj    '((:sdl-key-y quit-game-process "quit-game")
				 (t nil "opening"))))

  (setf (gethash "castle" *node-table*)
	(make-instance 'node
		       :message-key "castle"
		       :prompt-key "Store Leave"
		       :kpj '((:sdl-key-s process-store "store")
			      (:sdl-key-l nil "opening"))))

  (setf (gethash "quit-game" *node-table*)
	(make-instance 'node
		       :message-key ""
		       :prompt-key ""
		       :kpj '((t nil "opening"))))

;;;;;;;;;; Store Table ;;;;;;;;;;
  
  (setf (gethash "store" *node-table*)
	(make-instance 'node
		       :message-key "list-goods"
		       :prompt-key "Store Reception"
		       :kpj '((:sdl-key-b nil "select-item-buy")
			      (:sdl-key-l nil "leave-store"))))

  (setf (gethash "List-goods" *message-table*)
	(make-instance 'message
		       :text #'list-goods-in-store))
  
  (setf (gethash "Store Reception")
	(make-instance 'prompt
		       :ask '("ようこそ ご希望は?" "B)uy L)eave")))
  
  (setf (gethash "leave-store" *node-table*)
	(make-instance 'node
		       :message-key ""
		       :prompt-key "退店しますか?"
		       :kpj '((:sdl-key-y nil "castle")
			      (t nil "store"))))

  (setf (gethash "select-item-buy" *node-table*)
	(make-instance 'node
		       :message-key "List-goods"
		       :prompt-key "どれを買いますか?"
		       :kpj '((:sdl-key-1 nil))))
;;;; Message Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setf (gethash "opening" *message-table*)
	(make-instance 'message
		       :text "~%~%~%~%   * TINY GAME *"))

  (setf (gethash "castle" *message-table*)
	(make-instance 'message
		       :text "~%~%~%~%   ** CASTLE **"))

;;;; Prompt Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setf (gethash "Start Quit" *prompt-table*)
	(make-instance 'prompt
		       :ask '("S)tart Game   Q)uit Game")))
  
  (setf (gethash "ゲームを終了しますか?" *prompt-table*)
	(make-instance 'prompt
		       :ask '("ゲームを終了しますか?" "Y)es N)o")))
  
  (setf (gethash "Leave Castle" *prompt-table*)
	(make-instance 'prompt
		       :ask '("L)eave Castle")))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-node ()
  (let* ((n     (gethash *current-node* *node-table*))
	 (m-key (message-key n))
	 (p-key (prompt-key n)))

    (if (emptyp m-key)
	(setf *current-message* "")
	(setf *current-message* (text (gethash m-key *message-table*))))
    (if (emptyp p-key)
	(setf *current-prompt* "")
	(setf *current-prompt* (ask (gethash p-key *prompt-table*))))
    (setf *current-kpj* (kpj n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-message+prompt ()
  (let* ((n    (gethash *current-node* *node-table*)))
    (unless (emptyp *current-message*)
      (if (functionp *current-message*)
	  (funcall *current-message*)
	  (progn
	    (clear-message-box)
	    (draw-message *current-message*)))
    (unless (emptyp *current-prompt*)
      (clear-prompt-box)
      (draw-prompt-of-node n))
    (sdl:update-display)))

(defun draw-message-of-node (n)
  (draw-message (text (gethash (message-key n) *message-table*))))

(defun draw-prompt-of-node (n)
  (apply #'draw-prompt (ask (gethash (prompt-key n) *prompt-table*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-node (key)

  (format t "*current-node* ~a~%" *current-node*)
  (format t "*current-message* ~a~%" *current-message*)
  (format t "*current-prompt* ~a~%" *current-prompt*)
  
  (check-node)
  (draw-message+prompt)
  
  (dolist (i *current-kpj*)
    (if (eq (car i) t)
	(progn
	  (when (cadr i)
	    (funcall (cadr i) key))
	  (setf *current-node* (caddr i)))
	(when (sdl:key= key (car i))
	  (progn
	    (when (cadr i)
	      (funcall (cadr i) key))
	    (setf *current-node* (caddr i))))))
  
  (check-node)
  (draw-message+prompt))
