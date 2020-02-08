(cl:defpackage :synthia
  (:use :cl)
  (:export
   :gen-sine :gen-rand :gen-samples :note-to-freq
   :play-samples :play-sequence))

(in-package :synthia)

;; OpenAL helpers -------------------------------------------

(defconstant sample-rate 44100)
(defconstant sample-width 2)
(defconstant chunk-samples 256)

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

(defun gen-samples (gen samples)
  (let ((sample-array (make-array samples)))
    (loop for i from 0 to (1- samples)
          do (setf (aref sample-array i) (funcall gen i)))
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
