
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize ()

  (sdl:window +screen-width+ +screen-height+
	      :title-caption "test" ) 
  (setf (sdl:frame-rate) 20)
  
  (init-font)
  (init-bg "Background.png")
  (init-sp "Sprite.png" 16)

  (sdl:draw-surface-at-* *bg* 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quit-game-process (key)
  (setf *quit-game* t)
  (format t "quit-game-process -> *quit-game*=~a~%" *quit-game*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (sdl:with-init ()
    
    (initialize)
    (global-process *global-states* nil)
    (sdl:update-display)
    (sdl:with-events (:poll)
      (:quit-event() t)
      (:key-down-event (:key key)
		       (global-process *global-states* key)
		       (when (sdl:key= key :sdl-key-escape )
			 (draw-prompt "Hello, world!")
			 (sdl:update-display)
			 (sdl:push-quit-event)))
      (:idle ()
	     (draw-message-states *global-states*)
	     (draw-prompt-states *global-states*)
;	     (draw-field)
	     (sdl:update-display)))))

