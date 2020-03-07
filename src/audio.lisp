(in-package :synthia)

;; Audio Engine
;; -------------------------------------------------------------
(defclass audio-engine ()
  ((sample-rate :initform 22050 :allocation :class)
   (buffer-size :initform 512 :allocation :class)
   (buffer-count :initform 6 :allocation :class)
   (lock :initform (bt:make-lock) :accessor lock)
   (finished :initform nil :accessor finished)
   (audio-buffer :accessor audio-buffer)
   (al-device)
   (al-context)
   (al-source)
   (al-buffers)
   (voices :initform nil)
   (position :initform 0)
   (thread)))

(defmethod init ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers
               thread position finished buffer-count) engine
    (setf al-device (alc:open-device nil))
    (setf al-context (alc:create-context al-device))
    (alc:make-context-current al-context)
    (setf al-source (al:gen-source))
    (setf al-buffers (al:gen-buffers buffer-count))
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
    (alc:close-device al-device)
    (format t "audio thread finished~%")))

(defmethod wall-time ((engine audio-engine))
  (/ (slot-value engine 'position)
     (slot-value engine 'sample-rate)))

(defmethod start-sound ((engine audio-engine) voice)
  (bt:with-lock-held ((lock engine))
    (with-slots (voices) engine      
      (start voice (wall-time engine))
      (setf voices (cons voice voices)))))

(defmethod stop-sound ((engine audio-engine) voice)
  (bt:with-lock-held ((lock engine))
    (stop voice (wall-time engine))))

(defmethod fill-al-buffer ((engine audio-engine) buffer)
  (with-slots (voices position buffer-size sample-rate audio-buffer) engine
    (let* ((sample-width 2)
           (scale (1- (ash 1 (1- (* 8 sample-width))))))
      (dotimes (i buffer-size)
        (let ((amplitude (loop for voice in voices
                               sum (get-sample voice (wall-time engine)))))
          (setf (cffi:mem-aref audio-buffer :short i)
                (round (* scale (max -1 (min 1 amplitude))))))
        (incf position))
      (al:buffer-data buffer :mono16 audio-buffer
                      (* buffer-size sample-width) sample-rate))))

(defmethod stream-buffers ((engine audio-engine) buffers)
  (with-slots (al-source voices) engine
    ;; clean idle voices
    (setf voices (remove-if #'is-idle voices))
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
  (format t "audio thread started~%")
  (with-slots (audio-buffer buffer-size) engine
    (cffi:with-foreign-object (scoped-buffer :short buffer-size)
      (setf audio-buffer scoped-buffer)
      (with-slots (al-source al-buffers) engine
        (stream-buffers engine al-buffers))
      (loop
        (bt:with-lock-held ((lock engine))
          (if (finished engine)
              (return)
              (process-audio-buffers engine)))
        ;; (sleep 0.01)
        ))))

(defmethod process-audio-buffers ((engine audio-engine))
  (with-slots (al-device al-context al-source al-buffers position) engine
    (let ((processed (al:get-source al-source :buffers-processed)))
      (when (> processed 0)
        (let ((free-buffers (al:source-unqueue-buffers al-source processed)))
          ;; keep playing
          (stream-buffers engine free-buffers))))))


(defun fill-buffer (buffer data rate)
  (let* ((size (length data))
         (sample-width 2)
         (scale (1- (ash 1 (1- (* 8 sample-width))))))
    (cffi:with-foreign-object (device-array :short size)
      (loop for i below size
            do (setf (cffi:mem-aref device-array :short i)
                     (floor (* (elt data i) scale))))
      (al:buffer-data buffer :mono16 device-array
                      (* size sample-width) rate))))

(defun play-buffer (data rate)
  (let ((engine (make-instance 'audio-engine)))
    (init engine)
    (unwind-protect 
         (bt:with-lock-held ((lock engine))
           (al:with-buffer (buffer)
             (fill-buffer buffer data rate)
             (al:with-source (source)
               (al:source source :buffer buffer)
               (al:source-play source)
               (sleep (/ (length data) rate))
               (al:source-stop source))))
      (fini engine))))
