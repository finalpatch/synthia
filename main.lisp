(cl:defpackage :synthia
  (:use :cl)
  (:export
   :gen-sine :gen-rand :gen-samples :note-to-freq
   :play-samples :play-sequence))

(in-package :synthia)

;; OpenAL helpers -------------------------------------------

(defconstant sample-rate 44100)
(defconstant sample-width 2)
(defconstant chunk-samples 512)

(defun samples-duration (data)
  (/ (length data) sample-rate))

(defun fill-buffer (buffer data)
  "Fill an OpenAL buffer with a lisp array of numbers between 0.0 and 1.0"
  (let ((size (length data))
        (scale (1- (ash 1 (1- (* 8 sample-width))))))
    (cffi:with-foreign-object (device-array :short size)
      (loop for i below size
            do (setf (cffi:mem-aref device-array :short i)
                     (round (* (elt data i) scale))))
      (al:buffer-data buffer :mono16 device-array
                      (* size sample-width) sample-rate))))

(defmacro with-al-context (&body body)
  `(alc:with-device (device)
     (alc:with-context (context device)
       (alc:make-context-current context)
       ,@body)))

(defun play-samples-in-al-context (data)
  (al:with-source (source)
    (al:with-buffer (buffer)
      (fill-buffer buffer data)
      (al:source source :buffer buffer)
      (al:source-play source)
      (sleep (samples-duration data))
      (al:source-stop source))))

;; -------------------------------------------------------------

(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2))

(defconstant twelveth-root-of-2 (expt 2 (/ 1 12)))

(defun gen-sine (freq)
  (lambda (pos)
    (let ((time (/ pos sample-rate)))
      (sin (* freq 2 pi time)))))

(defun gen-square (freq)
  (let ((sine-generator (gen-sine freq)))
    (lambda (pos)
      (if (> (funcall sine-generator pos) 0)
          1.0
          0.0))))

(defun gen-rand (pos)
  (1- (random 2.0)))

(defun gen-samples (gen samples &optional (pos 0))
  (let ((sample-array (make-array samples)))
    (loop for i from 0 to (1- samples)
          do (setf (aref sample-array i) (funcall gen (+ pos i))))
    sample-array))

(defun note-to-freq (note)
  (if (equal note :R)
      0
      (let ((k (- (position note *music-scale*)
                  (position :A *music-scale*))))
        (* 440 (expt twelveth-root-of-2 k)))))

(defun note (name duration)
  (gen-samples (gen-sine (note-to-freq name))
               (floor (+ 0.5 (* duration sample-rate)))))

;; -------------------------------------------------------------

(defun play-samples (data)
  (with-al-context
      (play-samples-in-al-context data)))

(defun play-sequence (seq)
  (with-al-context
      (loop for e in seq
            do (play-samples-in-al-context
                (note (car e) (cadr e))))))

;; -------------------------------------------------------------

(defun fill-streaming-buffers (freq pos buffers)
  (loop for b in buffers
        do (let ((samples (gen-samples (gen-square freq) chunk-samples pos)))
             (fill-buffer b samples)
             (incf pos chunk-samples)))
  pos)

(defun keyboard ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))

      (with-al-context
        (al:with-source (source)
          (al:with-buffers (4 buffers)
            (let ((freq 0) (pos 0))

              ;; queue all buffers
              (setf pos (fill-streaming-buffers freq pos buffers))
              (al:source-queue-buffers source buffers)
              (al:source-play source)

              (format t "Beginning main loop.~%") (finish-output)
              (sdl2:with-event-loop  (:method :poll)
                (:keydown (:keysym keysym)
                          (let ((scancode (sdl2:scancode-value keysym))
                                (sym (sdl2:sym-value keysym))
                                (mod-value (sdl2:mod-value keysym)))
                            (cond
                              ((sdl2:scancode= scancode :scancode-w) (setf freq 440))
                              )
                            (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                    sym scancode mod-value)))
                (:keyup (:keysym keysym)
                        (setf freq 0)
                        (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                          (sdl2:push-event :quit)))
                (:idle ()
                       (let ((processed (al:get-source source :buffers-processed)))
                         (when (> processed 0)
                           (let ((free-buffers (al:source-unqueue-buffers source processed)))

                             ;; queue buffers
                             (setf pos (fill-streaming-buffers freq pos free-buffers))
                             (al:source-queue-buffers source free-buffers)
                             (format t "queue buffers ~a~%" processed) (finish-output)

                             (when (equal (al:get-source source :source-state) :stopped)
                               (setf pos 0)
                               (al:source-play source))
                             ))))
                (:quit () t))
              (format t "Exiting main loop.~%") (finish-output))))))))

;; -------------------------------------------------------------

;; (play-sequence '((:C 0.5)
;;                  (:C 0.5)
;;                  (:G 0.5)
;;                  (:G 0.5)
;;                  (:A 0.5)
;;                  (:A 0.5)
;;                  (:G 0.5)
;;                  (:R 0.5)
;;                  (:F 0.5)
;;                  (:F 0.5)
;;                  (:E 0.5)
;;                  (:E 0.5)
;;                  (:D 0.5)
;;                  (:D 0.5)
;;                  (:C 0.5)
;;                  ))

;; white noise
;; (play-samples (gen-samples #'gen-rand sample-rate))
;; 440hz
;; (play-samples (gen-samples (gen-square 220) sample-rate))
;; note A
;; (play-samples (note :A2 (/ 1 2)))
