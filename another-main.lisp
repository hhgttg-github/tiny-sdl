

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
       (case states ((castle key)
		     (store key)
		     (maze key)
		     (combat key)
		     (event key))))
      (:idle ()
	     (when *quit-game*
	       (sdl:push-quit-event))
	     (sdl:update-display)))))

