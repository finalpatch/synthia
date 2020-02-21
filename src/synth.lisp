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

   (attack-slope :initform 20)
   (decay-slope :initform -20)
   (sustain-level :initform 0.7)
   (release-slope :initform -0.3)

   (last-time :initform 0)
   ;; :attack :decay :sustain :release
   (phase :initform :idle)
   (level :initform 0)))

(defmethod start ((ins instrument) freq time)
  (with-slots (frequency phase last-time) ins
    (setf frequency freq)
    (setf last-time time)
    (setf phase :attack)))

(defmethod stop ((ins instrument) freq time)
  (with-slots (frequency phase) ins
    (when (= frequency freq)
      (setf phase :release))))

(defmethod get-slope ((ins instrument))
  (with-slots (phase attack-slope decay-slope release-slope) ins
    (cond
      ((eq phase :idle) 0)
      ((eq phase :attack) attack-slope)
      ((eq phase :decay) decay-slope)
      ((eq phase :sustain) 0)
      ((eq phase :release) release-slope))))

(defmethod envelop ((ins instrument) time)
  (with-slots (last-time level sustain-level phase) ins
    (let ((slope (get-slope ins))
          (delta-time (- time last-time)))
      (setf last-time time)
      (setf level (+ level (* delta-time slope)))
      (cond
        ;; attack->decay
        ((and (eq phase :attack) (> level 1))
         (setf level 1)
         (setf phase :decay))
        ;; decay->sustain
        ((and (eq phase :decay) (< level sustain-level))
         (setf level sustain-level)
         (setf phase :sustain))
        ;; release->idle
        ((and (eq phase :release) (< level 0))
         (setf level 0)
         (setf phase :idle)))
      level)))

(defmethod compute-sample ((ins instrument) time)
  (let ((freq (freq ins)))
    (* 0.5
       (envelop ins time)
       (+
        (* 0.4 (modulate (osc ins)
                         freq
                         #'osc-sine
                         5
                         0.001
                         time))
        (* 0.2  (osc-square (* freq 1.5) time))
        (* 0.1  (osc-square (* freq 2) time))
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
