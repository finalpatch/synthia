(in-package :synthia)

(defparameter *midi-file-path*
  (asdf:system-relative-pathname 'synthia #p"ff3shadw.mid"))

(defun key-to-freq (key)
  (let ((k (- key 69)))
    (* 440 (expt twelveth-root-of-2 k))))

(defparameter *keys* (make-hash-table))

(defun get-tone (key)
  (let ((tone (gethash key *keys*)))
    (unless tone
      (setf tone
            (setf (gethash key *keys*)
                  (make-instance 'bell :freq  (key-to-freq key)))))
    tone))

(defun wait-for-target-time (msg time-base)
  (let ((relative-time (- (get-internal-real-time) time-base))
        (target-time (midi:message-time msg)))
    (format t "target ~a current ~a ~%" target-time relative-time)
    (let ((delay (/ (- target-time relative-time) 1000)))
      (when (> delay 0)
        (sleep delay)))))

(defgeneric play-midi-msg (msg engine time-base)
  (:method (msg engine time-base)
    (format t "play ~a~%" msg))
  (:method ((msg midi:note-on-message) engine time-base)
    (wait-for-target-time msg time-base)
    (format t "key on: ~a~%" (midi:message-key msg))
    (start-sound engine 
                 (get-tone
                  (midi:message-key msg))))
  (:method ((msg midi:note-off-message) engine time-base)
    (wait-for-target-time msg time-base)
    (format t "key off: ~a~%" (midi:message-key msg))
    (stop-sound engine 
                (get-tone
                 (midi:message-key msg)))))

(defun play ()
  (let* ((tracks (midi:midifile-tracks (midi:read-midi-file *midi-file-path*)))
         (engine (make-instance 'audio-engine))
         (time-base (get-internal-real-time))
         ;; (playing-tracks (list (car tracks) (cadr tracks)))
         )
    (init engine)
    (unwind-protect
         (loop for msg in (caddr tracks)
               do (play-midi-msg msg engine time-base))
      (fini engine))))
