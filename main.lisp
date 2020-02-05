(in-package :synthia)

(defparameter *vendor*
  (alc:with-device (device)
    (alc:with-context (context device)
      (alc:make-context-current context)
      (al:get-string :vendor))))
