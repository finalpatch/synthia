(in-package :synthia)

(defvar *device* (alc:open-device nil))
(defvar *context* (alc:create-context *device*))
(alc:make-context-current *context*)

(format t "Device  : ~S~%" *device*)
(format t "Context : ~S~%" *context*)
(format t "Vendor  : ~A~%" (al:get-string :vendor))
(format t "Version : ~A~%" (al:get-string :version))
(format t "Renderer: ~A~%" (al:get-string :renderer))

(defparameter *sample-rate* 44100)
(defparameter *sample-size* 2)
(defparameter *sample-scale* 32767)
(defparameter *chunk-duration* 1)
(defparameter *chunk-size*
  (* *sample-rate*
     *sample-size*
     *chunk-duration*))

(defun dispose ()
  (when *context*
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context *context*)
    (makunbound '*context*))
  (when *device*
    (alc:close-device *device*)
    (makunbound '*device*)))

(defun fill-buffer (buffer data)
  (let ((size (length data)))
    (cffi:with-foreign-object (device-array :short size)
      (loop for i below size
            do (setf (cffi:mem-aref device-array :short i)
                     (floor (* (elt data i) *sample-scale*))))
      (al:buffer-data buffer :mono16 device-array
                      (* size *sample-size*) *sample-rate*))))

(defun test (data)
  (al:with-buffer (buffer)
    (fill-buffer buffer data)
    (al:with-source (source)
      (al:source source :buffer buffer)
      (al:source-play source)
      (sleep 1)
      (al:source-stop source))))

;; white noise
(test (aops:rand 44100))
