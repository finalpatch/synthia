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

(defun samples-duration (data)
  (/ (length data) *sample-rate*))

(defun play-samples-in-al-context (data)
  (al:with-source (source)
    (al:with-buffer (buffer)
      (fill-al-buffer buffer data)
      (al:source source :buffer buffer)
      (al:source-play source)
      (loop until (equal (al:get-source source :source-state) :stopped)
            do (sleep 0.01)))))

;; -------------------------------------------------------------

(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2))

(defconstant twelveth-root-of-2 (expt 2 (/ 1 12)))

(defun osc-random (pos)
  (declare (ignore pos))
  (1- (random 2.0)))

(defun osc-sine (freq)
  (lambda (pos)
    (let ((time (/ pos *sample-rate*)))
      (sin (* freq 2 pi time)))))

(defun osc-square (freq)
  (let ((sine-generator (osc-sine freq)))
    (lambda (pos)
      (if (> (funcall sine-generator pos) 0)
          1.0 -1.0))))

(defun gen-samples (osc samples &optional (pos 0))
  (let ((sample-array (make-array samples)))
    (loop for i from 0 to (1- samples)
          do (setf (aref sample-array i) (funcall osc (+ pos i))))
    sample-array))

(defun note-to-freq (note)
  (if (equal note :R)
      0
      (let ((k (- (position note *music-scale*)
                  (position :A *music-scale*))))
        (* 440 (expt twelveth-root-of-2 k)))))

(defun gen-note (name duration &optional (osc 'osc-sine))
  (gen-samples (funcall osc (note-to-freq name))
               (floor (+ 0.5 (* duration *sample-rate*)))))

;; -------------------------------------------------------------

(defun play-samples (data)
  (with-al-context
      (play-samples-in-al-context data)))

(defun play-sequence (seq)
  (with-al-context
      (loop for e in seq
            do (play-samples-in-al-context
                (gen-note (car e) (cadr e))))))

;; -------------------------------------------------------------

(defun stream-buffers (freq pos source buffers)
  ;; fill buffers with new samples
  (loop for b in buffers
        do (let ((samples (gen-samples (osc-square freq) *buffer-samples* pos)))
             (fill-al-buffer b samples)
             (incf pos *buffer-samples*)))
  ;; queue buffers on the source
  (al:source-queue-buffers source buffers)
  ;; start play if not already
  (unless (equal (al:get-source source :source-state) :playing)
    (al:source-play source))
  pos)

(defun keyboard ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))
      (with-al-context
        (al:with-source (source)
          (al:with-buffers (*buffer-count* buffers)
            (let ((freq 0) (pos 0))
              ;; start playing
              (setf pos (stream-buffers freq pos source buffers))
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
                             ;; keep playing
                             (setf pos (stream-buffers freq pos source free-buffers))
                             (format t "queue buffers ~a~%" processed) (finish-output)))))
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
;; (play-samples (gen-samples #'osc-random *sample-rate*))
;; 440hz
;; (play-samples (gen-samples (osc-square 220) *sample-rate*))
;; note A
;; (play-samples (gen-note :A (/ 1 2) 'osc-sine))
