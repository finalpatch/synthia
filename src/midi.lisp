(in-package :synthia)

(defparameter *midi-file-path*
  (asdf:system-relative-pathname 'synthia #p"ff3shadw.mid"))

(defun key-to-freq (key)
  (let ((k (- key 69)))
    (* 440 (expt twelveth-root-of-2 k))))

(defparameter *keys1* (make-hash-table))
(defparameter *keys2* (make-hash-table))

(defun get-tone (track key)
  (let* ((keys (if (eq track 1) *keys1* *keys2*))
         (tone (gethash key keys)))
    (unless tone
      (setf tone
            (setf (gethash key keys)
                  (make-instance 'bell :freq  (key-to-freq key)))))
    tone))

(defun wait-for-target-time (msg time-base)
  (let ((relative-time (- (get-internal-real-time) time-base))
        (target-time (midi:message-time msg)))
    (let ((delay (/ (- target-time relative-time)
                    INTERNAL-TIME-UNITS-PER-SECOND)))
      (when (> delay 0)
        (sleep delay)))))

(defgeneric play-midi-msg (msg track engine time-base)
  (:method (msg track engine time-base)
    (format t "play ~a~%" msg))
  (:method ((msg midi:note-on-message) track engine time-base)
    (wait-for-target-time msg time-base)
    (format t "key on: ~a~%" (midi:message-key msg))
    (start-sound engine 
                 (get-tone track
                  (midi:message-key msg))))
  (:method ((msg midi:note-off-message) track engine time-base)
    (wait-for-target-time msg time-base)
    (format t "key off: ~a~%" (midi:message-key msg))
    (stop-sound engine 
                (get-tone track
                 (midi:message-key msg)))))

(defun flatten-tracks (tracks)
  (let ((combined nil)
        (tid 0))
    (dolist (track tracks)
      (dolist (msg track)
        (when (and (or (= tid 1) (= tid 2))
                   (or (eq (type-of msg) 'midi:note-on-message)
                       (eq (type-of msg) 'midi:note-off-message)))
          (setf combined (cons (list tid msg) combined))))
      (incf tid))
    (sort combined (lambda (a b)
                     (< (midi:message-time (cadr a))
                        (midi:message-time (cadr b)))))))

(defun play ()
  (let* ((tracks (midi:midifile-tracks (midi:read-midi-file *midi-file-path*)))
         (engine (make-instance 'audio-engine))
         (time-base (get-internal-real-time)))
    (init engine)
    (unwind-protect
         (loop for tid-msg in (flatten-tracks tracks)
               do
                  (let ((tid (car tid-msg))
                        (msg (cadr tid-msg)))
                    (play-midi-msg tid msg engine time-base)))
      (fini engine))))
