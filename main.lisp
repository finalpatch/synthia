(cl:defpackage :synthia
  (:use :cl)
  (:export :keyboard))

(in-package :synthia)

(defparameter *sample-rate* 44100)
(defparameter *buffer-size* 512)
(defparameter *buffer-count* 6)

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
    (* 0.5
       (envelop ins time-since-start)
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

;; Audio Engine
;; -------------------------------------------------------------
(defvar *audio-buffer* nil)

(defclass audio-engine ()
  ((lock :initform (bt:make-lock) :accessor lock)
   (finished :initform nil :accessor finished)
   (al-device)
   (al-context)
   (al-source)
   (al-buffers)
   (instrument)
   (position :initform 0)
   (thread)))

(defmethod wall-time ((engine audio-engine))
  (/ (slot-value engine 'position) *sample-rate*))

(defmethod init ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers
               instrument thread position finished) engine
    (setf al-device (alc:open-device nil))
    (setf al-context (alc:create-context al-device))
    (alc:make-context-current al-context)
    (setf al-source (al:gen-source))
    (setf al-buffers (al:gen-buffers *buffer-count*))
    (setf instrument (make-instance 'instrument))
    (setf finished nil)
    (setf position 0)
    (setf thread (bt:make-thread (lambda () (audio-thread engine))))))

(defmethod fini ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers thread) engine
    (bt:with-lock-held ((lock engine))
      (setf (finished engine) t))
    (when (bt:thread-alive-p thread)
      (bt:join-thread thread))
    (al:delete-buffers al-buffers)
    (al:delete-source al-source)
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context al-context)
    (alc:close-device al-device)))

(defmethod start-sound ((engine audio-engine) freq)
  (bt:with-lock-held ((lock engine))
    (with-slots (instrument) engine
      (start instrument freq (wall-time engine)))))

(defmethod stop-sound ((engine audio-engine) freq)
  (bt:with-lock-held ((lock engine))
    (with-slots (instrument) engine
      (stop instrument freq (wall-time engine)))))

(defmethod fill-al-buffer ((engine audio-engine) buffer)
  (with-slots (instrument position) engine
    (let* ((sample-width 2)
           (scale (1- (ash 1 (1- (* 8 sample-width))))))
      (dotimes (i *buffer-size*)
        (setf (cffi:mem-aref *audio-buffer* :short i)
              (round (* scale (compute-sample instrument (wall-time engine)))))
        (incf position))
      (al:buffer-data buffer :mono16 *audio-buffer*
                      (* *buffer-size* sample-width) *sample-rate*))))

(defmethod stream-buffers ((engine audio-engine) buffers)
  (with-slots (al-source) engine
    ;; fill buffers with new samples
    (loop for b in buffers do
      (fill-al-buffer engine b))
    ;; queue buffers on the source
    (al:source-queue-buffers al-source buffers)
    ;; start play if not already
    (let ((state (al:get-source al-source :source-state)))
      (when (equal state :stopped) (format t "stutter~%"))
      (unless (equal state :playing) (al:source-play al-source)))))

(defmethod audio-thread ((engine audio-engine))
  (cffi:with-foreign-object (*audio-buffer* :short *buffer-size*)
    (with-slots (al-source al-buffers instrument) engine
      (stream-buffers engine al-buffers))
    (loop
      (bt:with-lock-held ((lock engine))
        (if (finished engine)
            (return)
            (process-audio-buffers engine)))
      (sleep 0.01))))

(defmethod process-audio-buffers ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers instrument position) engine
    (let ((processed (al:get-source al-source :buffers-processed)))
      (when (> processed 0)
        (let ((free-buffers (al:source-unqueue-buffers al-source processed)))
          ;; keep playing
          (stream-buffers engine free-buffers))))))

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
