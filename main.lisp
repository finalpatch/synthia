(cl:defpackage :synthia
  (:use :cl)
  (:export :keyboard))

(in-package :synthia)

(defparameter *sample-rate* 44100)
(defparameter *buffer-samples* 512)
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

;; (/ pos *sample-rate*))
(defun osc-sine (freq time)
    (sin (* freq 2 pi time)))

(defun osc-square (freq time)
  (if (> (osc-sine freq time) 0)
      1.0 -1.0))

(defun osc-saw (freq pos)
  (let ((period (/ 1 freq)))
    (1- (* 2 freq (mod pos period)))))

;; Musical notes
;; -------------------------------------------------------------
(defun gen-samples (synth freq samples &optional (pos 0))
  (let ((sample-array (make-array samples)))
    (dotimes (i samples)
      (setf (aref sample-array i)
            (funcall synth freq (/ (+ pos i) *sample-rate*))))
    sample-array))

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
(defun stream-buffers (synth freq pos source buffers)
  ;; fill buffers with new samples
  (loop for b in buffers do
    (let ((samples (gen-samples synth freq *buffer-samples* pos)))
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

(defun keyboard-loop (synth source buffers ren)
  (let ((freq 0)
        (pos 0)
        (tex (sdl2:create-texture-from-surface
              ren (sdl2-image:load-image "keyboard.png"))))
    ;; start playing
    (stream-buffers #'osc-zero freq pos source buffers)
    (format t "Beginning main loop.~%") (finish-output)
    (sdl2:with-event-loop  (:method :poll)
      (:keydown (:keysym keysym :repeat repeat)
                (when (= repeat 0)
                  (let ((scancode (sdl2:scancode-value keysym)))
                    (setf freq (note-to-freq (scancode-to-note scancode)))
                    (setf pos 0))))
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
                   (setf pos (stream-buffers
                              (if (> freq 0) synth #'osc-zero)
                              freq pos source free-buffers)))))
             (sdl2:render-clear ren)
             (sdl2:render-copy ren tex)
             (sdl2:render-present ren))
      (:quit () t))
    (format t "Exiting main loop.~%") (finish-output)))

(defun keyboard (&optional (synth 'osc-square))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        (with-al-context
            (al:with-source (source)
              (al:with-buffers (*buffer-count* buffers)
                (keyboard-loop synth source buffers ren))))))))
