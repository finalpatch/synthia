(cl:defpackage :synthia
  (:use :cl)
  (:export :keyboard))

(in-package :synthia)

(defparameter *sample-rate* 44100)
(defparameter *audio-buffer* (make-array 512))
(defparameter *buffer-count* 4)

;; OpenAL helpers
;; -------------------------------------------------------------
(defmacro with-al-context (&body body)
  `(alc:with-device (device)
     (alc:with-context (context device)
       (alc:make-context-current context)
       ,@body)))

(defun fill-al-buffer (buffer data)
  "Fill an OpenAL buffer with a lisp array of numbers between 0.0 and 1.0"
  (let* ((sample-width 2)
         (size (length data))
         (scale (1- (ash 1 (1- (* 8 sample-width))))))
    (cffi:with-foreign-object (device-array :short size)
      (dotimes (i size)
        (setf (cffi:mem-aref device-array :short i)
              (round (* (elt data i) scale))))
      (al:buffer-data buffer :mono16 device-array
                      (* size sample-width) *sample-rate*))))

;; Oscillators
;; -------------------------------------------------------------
(defun osc-zero (freq time)
  (declare (ignore freq time))
  0)

(defun osc-random (freq time)
  (declare (ignore freq time))
  (1- (random 2.0)))

(defun osc-sine (freq time)
    (sin (* freq 2 pi time)))

(defun osc-square (freq time)
  (if (> (osc-sine freq time) 0)
      1.0 -1.0))

(defun osc-triangle (freq time)
  (* (/ 2 pi) (asin (osc-sine freq time))))

(defun osc-saw (freq pos)
  (if (equal 0 freq)
      0
      (let ((period (/ 1 freq)))
        (1- (* 2 freq (mod pos period))))))

;; Frequency modulation
;; -------------------------------------------------------------
(defun modulate (carrier-osc carrier-freq
                 modulator-osc modulator-freq
                 modulation-index
                 time)
  (let ((modulation (funcall modulator-osc modulator-freq time)))
    (funcall carrier-osc carrier-freq
             (+ time (* modulation-index modulation)))))

;; Envelopes (ADSR)
;; -------------------------------------------------------------
(defclass instrument ()
  ((oscillator :initform 'osc-square :reader osc)
   (frequency :initform 0 :accessor freq)
   (start-time :initform 0 :accessor start-time)
   (stop-time :initform 0 :accessor stop-time)

   (attack-time :initform 0.05)
   (decay-time :initform 0.02)
   (sustain-amplitude :initform 0.7)
   (release-time :initform 0.2)

   (note-on :initform nil)))

(defmethod start ((ins instrument) freq time)
  (with-slots (frequency note-on start-time stop-time) ins
    (setf note-on t)
    (setf frequency freq)
    (setf start-time time)))

(defmethod stop ((ins instrument) freq time)
  (with-slots (frequency note-on stop-time) ins
    (when (equal freq frequency)
      (setf note-on nil)
      (setf stop-time time))))

(defmethod envelop ((ins instrument) time-since-start)
  (with-slots (note-on start-time stop-time
               attack-time decay-time sustain-amplitude release-time)
      ins
    (max 
     (if note-on
         (cond
           ;; attack
           ((< time-since-start attack-time)
            (/ time-since-start attack-time))
           ;; decay
           ((< time-since-start (+ attack-time decay-time));)
            (- 1 (* (- 1 sustain-amplitude)
                    (/ (- time-since-start attack-time) decay-time))))
           ;; sustain
           (t sustain-amplitude))
         ;; release
         (let ((time-since-stop
                 (- (+ time-since-start start-time) stop-time))
               (slope (/ sustain-amplitude release-time)))
           (- sustain-amplitude (* time-since-stop slope))))
     0)))

(defmethod compute-sample ((ins instrument) time)
  (let ((time-since-start (- time (start-time ins)))
        (freq (freq ins)))
    (* (envelop ins time-since-start)
       (+
        (* 0.4 (modulate (osc ins)
                  freq
                  #'osc-sine
                  5
                  0.001
                  time-since-start))
        (* 0.2  (osc-square (* freq 1.5) time-since-start))
        (* 0.1  (osc-square (* freq 2) time-since-start))
        (* 0.05 (osc-random 0 0))
        ))))

;; Musical notes
;; -------------------------------------------------------------
(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2
    :C3 :C3# :D3 :D3# :E3))

(defconstant twelveth-root-of-2 (expt 2 (/ 1 12)))

(defun note-to-freq (note)
  (if (equal note :R)
      0
      (let ((k (- (position note *music-scale*)
                  (position :A *music-scale*))))
        (* 440 (expt twelveth-root-of-2 k)))))

;; Keyboard
;; -------------------------------------------------------------
(defun pos-to-time (pos) (/ pos *sample-rate*))

(defun gen-samples (ins sample-array pos)
  (dotimes (i (length sample-array))
    (setf (aref sample-array i)
          (compute-sample ins (pos-to-time (+ pos i))))))

(defun stream-buffers (instrument pos source buffers)
  ;; fill buffers with new samples
  (loop for b in buffers do
    (gen-samples instrument *audio-buffer* pos)
    (fill-al-buffer b *audio-buffer*)
    (incf pos (length *audio-buffer*)))
  ;; queue buffers on the source
  (al:source-queue-buffers source buffers)
  ;; start play if not already
  (let ((state (al:get-source source :source-state)))
    (when (equal state :stopped) (format t "stutter~%") (finish-output))
    (unless (equal state :playing) (al:source-play source)))
  pos)

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
  (keyboard-map ((:z :C)
     (:x :D) (:c :E) (:v :F) (:b :G) (:n :A) (:m :B)
     (:s :C#) (:d :D#) (:g :F#) (:h :G#) (:j :A#)    
     (:q :C2) (:w :D2) (:e :E2) (:r :F2) (:t :G2) (:y :A2) (:u :B2)
     (:2 :C2#) (:3 :D2#) (:5 :F2#) (:6 :G2#) (:7 :A2#)    
     (:i :C3) (:o :D3) (:p :E3)    
     (:9 :C3#) (:0 :D3#))))

(defclass audio-engine ()
  ((lock :initform (bt:make-lock) :accessor lock)
   (al-device)
   (al-context)
   (al-source)
   (al-buffers)
   (instrument)
   (position)
   (thread)
   (finished :initform nil :accessor finished)))

(defmethod start-sound ((engine audio-engine) freq)
  (bt:with-lock-held ((lock engine))
    (with-slots (instrument position) engine
      (start instrument freq (pos-to-time position)))))

(defmethod stop-sound ((engine audio-engine) freq)
  (bt:with-lock-held ((lock engine))
    (with-slots (instrument position) engine
      (stop instrument freq (pos-to-time position)))))

(defmethod init ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers instrument position thread finished) engine
    (setf al-device (alc:open-device nil))
    (setf al-context (alc:create-context al-device))
    (alc:make-context-current al-context)
    (setf al-source (al:gen-source))
    (setf al-buffers (al:gen-buffers *buffer-count*))
    (setf instrument (make-instance 'instrument))
    (setf position (stream-buffers instrument 0 al-source al-buffers))
    (setf finished nil)
    (setf thread (bt:make-thread (lambda () (audio-thread engine))))))

(defmethod fini ((engine audio-engine))
    (bt:with-lock-held ((lock engine))
      (setf (finished engine) t))
    (bt:join-thread (slot-value engine 'thread))
  (with-slots (al-device al-context al-source al-buffers) engine
    (al:delete-buffers al-buffers)
    (al:delete-source al-source)
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context al-context)
    (alc:close-device al-device)))

(defmethod audio-thread ((engine audio-engine))
  (loop do
    (bt:with-lock-held ((lock engine))
      (if (finished engine)
          (return)
          (process-audio engine)))
    (sleep 0.01)))

(defmethod process-audio ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers instrument position) engine
    (let ((processed (al:get-source al-source :buffers-processed)))
      (when (> processed 0)
        (let ((free-buffers (al:source-unqueue-buffers al-source processed)))
          ;; keep playing
          (setf position (stream-buffers instrument position al-source free-buffers)))))))

(defclass keyboard-window (sdl2.kit:window)
  ((renderer)
   (texture)
   (al-device)
   (al-context)
   (al-source)
   (al-buffers)
   (ins)
   (position :initform 0)
   (timer)))

(cffi:defcallback timercb :uint32 ((interval :uint32) (param :pointer))
  (let* ((window-id (cffi:pointer-address param))
         (window (sdl2.kit:window-from-id window-id)))
    (with-slots (al-source ins position) window
      (let ((processed (al:get-source al-source :buffers-processed)))
        (when (> processed 0)
          (let ((free-buffers (al:source-unqueue-buffers al-source processed)))
            ;; (format t "~a free buffers~%" free-buffers)
            ;; keep playing
            (setf position (stream-buffers ins position al-source free-buffers))
            )))))
  10)

(defmethod sdl2.kit:initialize-window progn ((window keyboard-window) &key &allow-other-keys)
  (with-slots (sdl2.kit::sdl-window renderer texture
               al-device al-context al-source al-buffers
               ins position timer) window
    (setf renderer (sdl2:create-renderer sdl2.kit::sdl-window nil '(:accelerated :presentvsync)))
    (setf texture (sdl2:create-texture-from-surface renderer (sdl2-image:load-image "keyboard.png")))
    (setf al-device (alc:open-device nil))
    (setf al-context (alc:create-context al-device))
    (alc:make-context-current al-context)
    (setf al-source (al:gen-source))
    (setf al-buffers (al:gen-buffers *buffer-count*))
    (setf ins (make-instance 'instrument))
    (setf position (stream-buffers ins 0 al-source al-buffers))
    (setf timer (sdl2:add-timer 10 (cffi:callback timercb)
                                (cffi:make-pointer (sdl2.kit:sdl-window-id window)))))
  (format t "Keyboard window initialized~%"))

(defmethod sdl2.kit:close-window ((window keyboard-window))
  (with-slots (renderer texture al-device al-context al-source al-buffers timer) window
    (sdl2:remove-timer timer)
    (sdl2:destroy-texture texture)
    (sdl2:destroy-renderer renderer)
    (al:delete-buffers al-buffers)
    (al:delete-source al-source)
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context al-context)
    (alc:close-device al-device))
  (format t "Keyboard window closed~%")
  (call-next-method))

(defmethod sdl2.kit:render :after ((window keyboard-window))
  (with-slots (renderer texture) window
    (sdl2:render-clear renderer)
    (sdl2:render-copy renderer texture)
    (sdl2:render-present renderer)))

(defmethod sdl2.kit:keyboard-event ((window keyboard-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode-value keysym)))
    (with-slots (ins position) window

      (cond
        ((sdl2:scancode= scancode :scancode-escape)
         (sdl2.kit:close-window window))
        
        ((and (eq state :keydown) (not repeat-p))
         (start ins
                (note-to-freq (scancode-to-note scancode))
                (pos-to-time position)))
        ((eq state :keyup)
         (stop ins
               (note-to-freq (scancode-to-note scancode))
               (pos-to-time position)))
      ))))

(sdl2.kit:define-start-function keyboard (&key (w 800) (h 600))
  (make-instance 'keyboard-window :w w :h h))
