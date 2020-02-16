(in-package :synthia)

;; Keyboard
;; -------------------------------------------------------------
(defclass keyboard-window (sdl2.kit:window)
  ((renderer) (texture) (engine)))

(defmethod sdl2.kit:initialize-window progn ((window keyboard-window) &key &allow-other-keys)
  (with-slots (sdl2.kit::sdl-window renderer texture engine) window
    (setf renderer (sdl2:create-renderer sdl2.kit::sdl-window nil '(:accelerated :presentvsync)))
    (setf texture (sdl2:create-texture-from-surface renderer (sdl2-image:load-image "keyboard.png")))
    (setf engine (make-instance 'audio-engine))
    (init engine))
  (format t "Keyboard window initialized~%"))

(defmethod sdl2.kit:close-window ((window keyboard-window))
  (with-slots (renderer texture engine) window
    (sdl2:destroy-texture texture)
    (sdl2:destroy-renderer renderer)
    (fini engine))
  (format t "Keyboard window closed~%")
  (call-next-method))

(defmethod sdl2.kit:render :after ((window keyboard-window))
  (with-slots (renderer texture) window
    (sdl2:render-clear renderer)
    (sdl2:render-copy renderer texture)
    (sdl2:render-present renderer)))

(defmacro keyboard-map (kbmap)
  (cons 'cond (append
               (loop for entry in kbmap
                     collect
                     `((sdl2:scancode=
                        scancode
                        ,(intern (concatenate 'string "SCANCODE-"
                                              (string-upcase (car entry)))
                                 :keyword))
                       ,(cadr entry)))
               '((t :R)))))

(defun scancode-to-note (scancode)
  (keyboard-map ((:z :C) (:x :D) (:c :E) (:v :F) (:b :G) (:n :A) (:m :B)
                 (:s :C#) (:d :D#) (:g :F#) (:h :G#) (:j :A#)
                 (:q :C2) (:w :D2) (:e :E2) (:r :F2) (:t :G2) (:y :A2) (:u :B2)
                 (:2 :C2#) (:3 :D2#) (:5 :F2#) (:6 :G2#) (:7 :A2#)
                 (:i :C3) (:o :D3) (:p :E3)
                 (:9 :C3#) (:0 :D3#))))

(defmethod sdl2.kit:keyboard-event ((window keyboard-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        (engine (slot-value window 'engine)))
    (cond
      ((sdl2:scancode= scancode :scancode-escape)
       (sdl2.kit:close-window window))
      ((and (eq state :keydown) (not repeat-p))
       (start-sound engine (note-to-freq (scancode-to-note scancode))))
      ((eq state :keyup)
       (stop-sound engine (note-to-freq (scancode-to-note scancode)))))))

(sdl2.kit:define-start-function keyboard (&key (w 800) (h 600))
  (make-instance 'keyboard-window :w w :h h))
