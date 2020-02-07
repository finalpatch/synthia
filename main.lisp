(in-package :synthia)

(defvar *device* (alc:open-device nil))
(defvar *context* (alc:create-context *device*))
(alc:make-context-current *context*)

(princ "OpenAL context initialized.")
(format t "Device  : ~a~%" *device*)
(format t "Context : ~a~%" *context*)
(format t "Vendor  : ~a~%" (al:get-string :vendor))
(format t "Version : ~a~%" (al:get-string :version))
(format t "Renderer: ~a~%" (al:get-string :renderer))

(defparameter *sample-rate* 44100)
(defparameter *sample-width* 2)
(defparameter *chunk-samples* 256)

(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2))

(defconstant twelveth-root-of-2
  (expt 2 (/ 1 12)))

(defun sample-scale ()
  (1- (ash 1 (1- (* 8 *sample-width*)))))

(defun dispose ()
  "Destroy and unbound the global OpenAL *device* and *context*"
  (when *context*
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context *context*)
    (makunbound '*context*))
  (when *device*
    (alc:close-device *device*)
    (makunbound '*device*)))

(defun fill-buffer (buffer data)
  "Fill an OpenAL buffer with a lisp array of numbers between 0.0 and 1.0"
  (let ((size (length data))
        (scale (sample-scale)))
    (cffi:with-foreign-object (device-array :short size)
      (loop for i below size
            do (setf (cffi:mem-aref device-array :short i)
                     (round (* (elt data i) scale))))
      (al:buffer-data buffer :mono16 device-array
                      (* size *sample-width*) *sample-rate*))))

(defun array-duration (data)
  (/ (length data) *sample-rate*))

(defun gen-sine (freq)
  (lambda (pos)
    (let ((time (/ pos *sample-rate*)))
      (sin (* freq 2 pi time)))))

(defun gen-rand (pos)
  (1- (random 2.0)))

(defun gen-samples (gen samples)
  (let ((sample-array (make-array samples)))
    (loop for i from 0 to (1- samples)
          do (setf (aref sample-array i) (funcall gen i)))
    sample-array))

(defun note-to-freq (note)
  (let ((k (- (position note *music-scale*)
              (position :A *music-scale*))))
    (* 440 (expt twelveth-root-of-2 k))))

(defun play-array (data)
  (al:with-buffer (buffer)
    (fill-buffer buffer data)
    (al:with-source (source)
      (al:source source :buffer buffer)
      (al:source-play source)
      (sleep (array-duration data))
      (al:source-stop source))))

;; white noise
;; (play-array (gen-samples #'gen-rand 44100))
;; 440hz
;; (play-array (gen-samples (gen-sine 440) 44100))
;; note A
;; (play-array (gen-samples (gen-sine (note-to-freq :A2)) 44100))
