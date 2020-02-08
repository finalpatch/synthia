(cl:defpackage :synthia
  (:use :cl)
  (:export
   :gen-sine :gen-rand :gen-samples :note-to-freq
   :play-samples :play-sequence :keyboard))

(in-package :synthia)

(defparameter *sample-rate* 44100)
(defparameter *buffer-samples* 512)
(defparameter *buffer-count* 4)

;; OpenAL helpers -------------------------------------------

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
      (loop for i below size
            do (setf (cffi:mem-aref device-array :short i)
                     (round (* (elt data i) scale))))
      (al:buffer-data buffer :mono16 device-array
                      (* size sample-width) *sample-rate*))))

;; -------------------------------------------------------------

(defun osc-zero (freq pos)
  (declare (ignore freq pos))
  0)

(defun osc-random (freq pos)
  (declare (ignore freq pos))
  (1- (random 2.0)))

(defun osc-sine (freq pos)
  (let ((time (/ pos *sample-rate*)))
    (sin (* freq 2 pi time))))

(defun osc-square (freq pos)
  (if (> (osc-sine freq pos) 0)
      1.0 -1.0))

;; -------------------------------------------------------------

(defun gen-samples (synth freq samples &optional (pos 0))
  (let ((sample-array (make-array samples)))
    (loop for i from 0 to (1- samples)
          do (setf (aref sample-array i) (funcall synth freq (+ pos i))))
    sample-array))

(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2))

(defconstant twelveth-root-of-2 (expt 2 (/ 1 12)))

(defun note-to-freq (note)
  (if (equal note :R)
      0
      (let ((k (- (position note *music-scale*)
                  (position :A *music-scale*))))
        (* 440 (expt twelveth-root-of-2 k)))))

;; -------------------------------------------------------------

(defun stream-buffers (synth freq pos source buffers)
  ;; fill buffers with new samples
  (loop for b in buffers
        do (let ((samples (gen-samples synth freq *buffer-samples* pos)))
             (fill-al-buffer b samples)
             (incf pos *buffer-samples*)))
  ;; queue buffers on the source
  (al:source-queue-buffers source buffers)
  ;; start play if not already
  (unless (equal (al:get-source source :source-state) :playing)
    (al:source-play source))
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
    ((sdl2:scancode= scancode :scancode-q) :C2)
    ((sdl2:scancode= scancode :scancode-w) :D2)
    ((sdl2:scancode= scancode :scancode-e) :E2)
    ((sdl2:scancode= scancode :scancode-r) :F2)
    ((sdl2:scancode= scancode :scancode-t) :G2)
    ((sdl2:scancode= scancode :scancode-y) :A2)
    ((sdl2:scancode= scancode :scancode-u) :B2)
    (t :R)))

(defun keyboard (&optional (synth 'osc-square))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))
      (with-al-context
        (al:with-source (source)
          (al:with-buffers (*buffer-count* buffers)
            (let ((freq 0) (pos 0))
              ;; start playing
              (setf pos (stream-buffers synth freq pos source buffers))
              (format t "Beginning main loop.~%") (finish-output)
              (sdl2:with-event-loop  (:method :poll)
                (:keydown (:keysym keysym)
                          (let ((scancode (sdl2:scancode-value keysym)))
                            (setf freq (note-to-freq (scancode-to-note scancode)))))
                (:keyup (:keysym keysym)
                        (let ((scancode (sdl2:scancode-value keysym)))
                          ;; if equal to last pressed key, set to silence
                          (when (= freq (note-to-freq (scancode-to-note scancode)))
                            (setf freq 0)))
                        (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                          (sdl2:push-event :quit)))
                (:idle ()
                       (let ((processed (al:get-source source :buffers-processed)))
                         (when (> processed 0)
                           (let ((free-buffers (al:source-unqueue-buffers source processed)))
                             ;; keep playing
                             (setf pos (stream-buffers synth freq pos source free-buffers))
                             (format t "queue buffers ~a~%" processed) (finish-output)))))
                (:quit () t))
              (format t "Exiting main loop.~%") (finish-output))))))))
