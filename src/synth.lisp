(in-package :synthia)

;; Oscillators
;; -------------------------------------------------------------
(defun osc-zero (freq time)
  (declare (ignore freq time))
  0)

(defun osc-random (freq time)
  (declare (ignore freq time))
  (1- (random 2.0)))

(defun osc-sine (freq time)
  (sin (* freq 2 pi time)))

(defun osc-square (freq time)
  (if (> (osc-sine freq time) 0)
      1.0 -1.0))

(defun osc-triangle (freq time)
  (* (/ 2 pi) (asin (osc-sine freq time))))

(defun osc-saw (freq pos)
  (if (equal 0 freq)
      0
      (let ((period (/ 1 freq)))
        (1- (* 2 freq (mod pos period))))))

;; Frequency modulation
;; -------------------------------------------------------------
(defun modulate (carrier-osc carrier-freq
                 modulator-osc modulator-freq
                 modulation-index
                 time)
  (let ((modulation (funcall modulator-osc modulator-freq time)))
    (funcall carrier-osc carrier-freq
             (+ time (* modulation-index modulation)))))

;; Envelopes (ADSR)
;; -------------------------------------------------------------
(defclass instrument ()
  ((oscillator :initform 'osc-square :reader osc)
   (frequency :initform 0 :accessor freq)
   (start-time :initform 0 :accessor start-time)
   (stop-time :initform 0 :accessor stop-time)

   (attack-time :initform 0.05)
   (decay-time :initform 0.02)
   (sustain-amplitude :initform 0.7)
   (release-time :initform 0.2)

   (note-on :initform nil)))

(defmethod start ((ins instrument) freq time)
  (with-slots (frequency note-on start-time stop-time) ins
    (setf note-on t)
    (setf frequency freq)
    (setf start-time time)))

(defmethod stop ((ins instrument) freq time)
  (with-slots (frequency note-on start-time stop-time attack-time decay-time) ins
    (let ((min-stop-time (+ start-time attack-time decay-time)))
      (when (equal freq frequency)
        (setf note-on nil)
        (setf stop-time (max min-stop-time time))))))

(defmethod envelop ((ins instrument) time-since-start)
  (with-slots (note-on start-time stop-time
               attack-time decay-time sustain-amplitude release-time)
      ins
    (max
         (let ((time-since-stop
                 (max 0 (- (+ time-since-start start-time) stop-time)))
               (slope (/ sustain-amplitude release-time)))
           (if (or note-on (= time-since-stop 0))
               (cond
                 ;; attack
                 ((< time-since-start attack-time)
                  (/ time-since-start attack-time))
                 ;; decay
                 ((< time-since-start (+ attack-time decay-time)) ;)
                  (- 1 (* (- 1 sustain-amplitude)
                          (/ (- time-since-start attack-time) decay-time))))
                 ;; sustain
                 (t sustain-amplitude))
               ;; release
               (- sustain-amplitude (* time-since-stop slope))))
     0)))

(defmethod compute-sample ((ins instrument) time)
  (let ((time-since-start (- time (start-time ins)))
        (freq (freq ins)))
    (* 0.5
       (envelop ins time-since-start)
       (+
        (* 0.4 (modulate (osc ins)
                         freq
                         #'osc-sine
                         5
                         0.001
                         time-since-start))
        (* 0.2  (osc-square (* freq 1.5) time-since-start))
        (* 0.1  (osc-square (* freq 2) time-since-start))
        (* 0.05 (osc-random 0 0))
        ))))

;; Musical notes
;; -------------------------------------------------------------
(defparameter *music-scale*
  #(:C  :C#  :D  :D#  :E  :F  :F#  :G  :G#  :A  :A#  :B
    :C2 :C2# :D2 :D2# :E2 :F2 :F2# :G2 :G2# :A2 :A2# :B2
    :C3 :C3# :D3 :D3# :E3))

(defconstant twelveth-root-of-2 (expt 2 (/ 1 12)))

(defun note-to-freq (note)
  (if (equal note :R)
      0
      (let ((k (- (position note *music-scale*)
                  (position :A *music-scale*))))
        (* 440 (expt twelveth-root-of-2 k)))))
