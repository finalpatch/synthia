(cl:defpackage :synthia
  (:use :cl)
  (:export :keyboard))

(in-package :synthia)

(defparameter *sample-rate* 44100)
(defparameter *audio-buffer* (make-array 512))
(defparameter *buffer-count* 6)

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

(defun scancode-to-note (scancode)
  (cond
    ((sdl2:scancode= scancode :scancode-z) :C)
    ((sdl2:scancode= scancode :scancode-x) :D)
    ((sdl2:scancode= scancode :scancode-c) :E)
    ((sdl2:scancode= scancode :scancode-v) :F)
    ((sdl2:scancode= scancode :scancode-b) :G)
    ((sdl2:scancode= scancode :scancode-n) :A)
    ((sdl2:scancode= scancode :scancode-m) :B)

    ((sdl2:scancode= scancode :scancode-s) :C#)
    ((sdl2:scancode= scancode :scancode-d) :D#)
    ((sdl2:scancode= scancode :scancode-g) :F#)
    ((sdl2:scancode= scancode :scancode-h) :G#)
    ((sdl2:scancode= scancode :scancode-j) :A#)
    
    ((sdl2:scancode= scancode :scancode-q) :C2)
    ((sdl2:scancode= scancode :scancode-w) :D2)
    ((sdl2:scancode= scancode :scancode-e) :E2)
    ((sdl2:scancode= scancode :scancode-r) :F2)
    ((sdl2:scancode= scancode :scancode-t) :G2)
    ((sdl2:scancode= scancode :scancode-y) :A2)
    ((sdl2:scancode= scancode :scancode-u) :B2)

    ((sdl2:scancode= scancode :scancode-2) :C2#)
    ((sdl2:scancode= scancode :scancode-3) :D2#)
    ((sdl2:scancode= scancode :scancode-5) :F2#)
    ((sdl2:scancode= scancode :scancode-6) :G2#)
    ((sdl2:scancode= scancode :scancode-7) :A2#)
    
    ((sdl2:scancode= scancode :scancode-i) :C3)
    ((sdl2:scancode= scancode :scancode-o) :D3)
    ((sdl2:scancode= scancode :scancode-p) :E3)
    
    ((sdl2:scancode= scancode :scancode-9) :C3#)
    ((sdl2:scancode= scancode :scancode-0) :D3#)

    (t :R)))

(defun keyboard-loop (source buffers ren)
  (let ((instrument (make-instance 'instrument))
        (pos 0)
        (tex (sdl2:create-texture-from-surface
              ren (sdl2-image:load-image "keyboard.png"))))
    ;; start playing
    (setf pos (stream-buffers instrument pos source buffers))
    (format t "Beginning main loop.~%") (finish-output)
    (sdl2:with-event-loop  (:method :poll)
      (:keydown (:keysym keysym :repeat repeat)
                (when (= repeat 0)
                  (let ((scancode (sdl2:scancode-value keysym)))
                    (start instrument
                           (note-to-freq (scancode-to-note scancode))
                           (pos-to-time pos)))))
      (:keyup (:keysym keysym)
              (let ((scancode (sdl2:scancode-value keysym)))
                (stop instrument
                      (note-to-freq (scancode-to-note scancode))
                      (pos-to-time pos)))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
      (:idle ()
             (let ((processed (al:get-source source :buffers-processed)))
               (when (> processed 0)
                 (let ((free-buffers (al:source-unqueue-buffers source processed)))
                   ;; keep playing
                   (setf pos (stream-buffers instrument pos source free-buffers)))))
             (sdl2:render-clear ren)
             (sdl2:render-copy ren tex)
             (sdl2:render-present ren))
      (:quit () t))
    (format t "Exiting main loop.~%") (finish-output)))

(defun keyboard ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        (with-al-context
            (al:with-source (source)
              (al:with-buffers (*buffer-count* buffers)
                (keyboard-loop source buffers ren))))))))
