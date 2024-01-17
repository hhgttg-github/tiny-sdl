
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
    (initialize-tables)
    
    (process-node nil)
    (sdl:update-display)

    (sdl:with-events (:poll)
      (:quit-event() t)
      (:key-down-event
       (:key key)
       (process-node key))
 		       ;; (when (sdl:key= key :sdl-key-escape )
		       ;; 	 (draw-prompt "Hello, world!")
		       ;; 	 (sleep 1)
		       ;; 	 (sdl:push-quit-event)))
      (:idle ()
	     (when *quit-game*
	       (sdl:push-quit-event))
	     (sdl:update-display)))))

