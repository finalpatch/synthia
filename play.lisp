(in-package :synthia)

(defun gen-byte-buffer (func time sample-rate)
  (let ((buffer (make-array (* 2 (floor (* time sample-rate)))
                            :element-type '(unsigned-byte 8)))
        (scale (1- (ash 1 (1- (* 8 2))))))
    (dotimes (i (/ (length buffer) 2))
      (let ((short-value (round (* scale (max -1 (min 1 (funcall func (/ i sample-rate))))))))
        (setf (aref buffer (* 2 i)) (logand short-value 255) )
        (setf (aref buffer (1+  (* 2 i))) (logand 255 (ash short-value -8)))))
    buffer))

(defun gen-samples (func time sample-rate)
  (let ((buffer (make-array (floor (* time sample-rate)))))
    (dotimes (i (length buffer))
      (setf (aref buffer i) (funcall func (/ i sample-rate))))
    buffer))

(defun to-short-buffer (buffer)
  (let* ((sample-width 2)
         (scale (1- (ash 1 (1- (* 8 sample-width))))))
    (map 'vector (lambda (x) (round (* scale (max -1 (min 1 x))))) buffer)))

(defun to-riff-chunks (buffer sample-rate channels)
  (let ((riff-size (+ 4                 ; file-type
                      4                 ; fmt header
                      16                ; fmt block
                      4                 ; data header
                      (length buffer)   ; data
                      ))
        (block-align (* 2 channels))
        (bytes-per-sec (* sample-rate channels 2)))
    `((:chunk-id "RIFF" :chunk-data-size ,riff-size :file-type "WAVE")
      (:chunk-id "fmt " :chunk-data-size 16 :chunk-data
                 (:compression-code 1
                  :number-of-channels ,channels
                  :sample-rate ,sample-rate
                  :average-bytes-per-second ,bytes-per-sec
                  :block-align ,block-align
                  :significant-bits-per-sample 16))
      (:CHUNK-ID "data" :CHUNK-DATA-SIZE ,(length buffer) :CHUNK-DATA
                 ,buffer))))

(defun karplus_strong (freq time rate)
  (let* ((wavetable (gen-samples (lambda (time) (if (> 1 (random 2.0)) 1 -1)) (/ (floor rate freq) rate) rate))
         (output (make-array (floor (* time rate))))
         (previous 0))
    (dotimes (i (length output))
      (let* ((wtidx (mod i (length wavetable)))
             (sample (* 0.5 (+ previous (aref wavetable wtidx)))))
        (setf (aref output i) sample)
        (setf (aref wavetable wtidx) sample)
        (setf previous sample)))
    output))

;; (wav:write-wav-file
;;  (to-riff-chunks (gen-byte-buffer
;;                   (lambda (time) (synthia::osc-sine 220 time)) 1 22050)
;;                  22050 1)
;;  "test.wav")
