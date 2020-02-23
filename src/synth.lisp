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

(defun osc-saw (freq time)
  (if (equal 0 freq)
      0
      (let ((period (/ 1 freq)))
        (1- (* 2 freq (mod time period))))))

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
  ((frequency :initform 0 :reader freq)
   (attack-time :initform 1)
   (decay-time :initform 1)
   (sustain-level :initform 1)
   (release-time :initform 1)

   (start-time :initform 0)
   (last-time :initform 0)
   (phase :initform :idle)
   (level :initform 0)))

(defmethod attack-slope ((ins instrument))
  (with-slots (attack-time) ins
    (/ 1 attack-time)))

(defmethod decay-slope ((ins instrument))
  (with-slots (decay-time sustain-level) ins
    (/ (- sustain-level 1) decay-time)))

(defmethod sustain-level ((ins instrument))
  (with-slots (sustain-level) ins
    sustain-level))

(defmethod release-slope ((ins instrument))
  (with-slots (sustain-level release-time) ins
    (/ (- sustain-level) release-time)))

(defmethod get-sample ((ins instrument) time)
  (let ((time-since-start (- time (slot-value ins 'start-time))))
    (compute-sample ins time-since-start)))
  
(defgeneric compute-sample (ins time))

(defmethod start ((ins instrument) time)
  (with-slots (frequency phase last-time attack-time level start-time) ins
    (setf last-time 0)
    (setf start-time time)
    (if (> attack-time 0)
        (setf phase :attack)
        (progn 
          (setf phase :decay)
          (setf level 1)))))

(defmethod stop ((ins instrument) time)
  (with-slots (phase release-time level) ins
    (if (> release-time 0)
        (setf phase :release)
        (progn
          (setf phase :idle)
          (setf level 0)))))

(defmethod get-slope ((ins instrument))
  (with-slots (phase) ins
    ;; (format t "phase ~a~%" phase)
    (cond
      ((eq phase :idle) 0)
      ((eq phase :attack) (attack-slope ins))
      ((eq phase :decay) (decay-slope ins))
      ((eq phase :sustain) 0)
      ((eq phase :release) (release-slope ins)))))

(defmethod envelop ((ins instrument) time)
  (with-slots (last-time level phase) ins
    (let ((slope (get-slope ins))
          (delta-time (- time last-time)))
      (setf last-time time)
      ;; (format t "~a ~a~%" level slope)
      (setf level (+ level (* delta-time slope)))
      (cond
        ;; attack->decay
        ((and (eq phase :attack) (>= level 1))
         ;; (format t "attack-decay~%")
         (setf level 1)
         (setf phase :decay))
        ;; decay->sustain
        ((and (eq phase :decay) (<= level (sustain-level ins)))
         ;; (format t "decay->sustain~%")
         (setf level (sustain-level ins))
         (setf phase :sustain))
        ;; release->idle
        ((and (eq phase :release) (<= level 0))
         ;; (format t "release->idle~%")
         (setf level 0)
         (setf phase :idle)))
      level)))

(defmethod is-idle ((ins instrument))
  (eq :idle (slot-value ins 'phase)))

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

;; Instrument classes
;; -------------------------------------------------------------
(defclass hamonica (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0)
   (decay-time :initform 1)
   (sustain-level :initform 0.95)
   (release-time :initform 1)))

(defmethod compute-sample ((ins hamonica) time)
  (with-slots (frequency) ins
    (* 0.25
       (envelop ins time)
       (+
        (modulate #'osc-saw (/ frequency 2) #'osc-sine 5 0.001 time)
        (modulate #'osc-square frequency #'osc-sine 5 0.001 time)
        (* 0.5  (osc-square (* frequency 2) time))
        (* 0.05 (osc-random 0 0))))))
;; -------------------------------------------------------------
(defclass bell (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0.01)
   (decay-time :initform 1.0)
   (sustain-level :initform 0.0)
   (release-time :initform 1)))

(defmethod compute-sample ((ins bell) time)
  (with-slots (frequency) ins
    (* 0.5
       (envelop ins time)
       (+
        (modulate #'osc-sine frequency #'osc-sine 5 0.001 time)
        (* 0.5  (osc-sine (* frequency 3) time))
        (* 0.25 (osc-sine (* frequency 4) time))))))
;; -------------------------------------------------------------
(defclass bell8 (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0.01)
   (decay-time :initform 0.5)
   (sustain-level :initform 0.8)
   (release-time :initform 1)))

(defmethod compute-sample ((ins bell8) time)
  (with-slots (frequency) ins
    (* 0.5
       (envelop ins time)
       (+
        (modulate #'osc-square frequency #'osc-sine 5 0.001 time)
        (* 0.5  (osc-sine (* frequency 3) time))
        (* 0.25 (osc-sine (* frequency 4) time))))))
;; -------------------------------------------------------------
(defclass drum-kick (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0.01)
   (decay-time :initform 0.15)
   (sustain-level :initform 0)
   (release-time :initform 0)))

(defmethod compute-sample ((ins drum-kick) time)
  (with-slots (frequency start-time) ins
    (* 0.5
       (envelop ins time)
       (+
        (* 0.99 (modulate #'osc-sine (/ frequency 8) #'osc-sine 1.0 1.0 time))
        (* 0.01 (osc-random 0 0))))))
;; -------------------------------------------------------------
(defclass drum-snare (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0)
   (decay-time :initform 0.2)
   (sustain-level :initform 0)
   (release-time :initform 0)))

(defmethod compute-sample ((ins drum-snare) time)
  (with-slots (frequency start-time) ins
    (* 0.5
       (envelop ins time)
       (+
        (* 0.5 (modulate #'osc-sine (/ frequency 4) #'osc-sine .5 1.0 time))
        (* 0.5 (osc-random 0 0))))))
;; -------------------------------------------------------------
(defclass drum-hihat (instrument)
  ((frequency :initform 0 :initarg :freq)
   (attack-time :initform 0.01)
   (decay-time :initform 0.05)
   (sustain-level :initform 0)
   (release-time :initform 0)))

(defmethod compute-sample ((ins drum-hihat) time)
  (with-slots (frequency start-time) ins
    (* 0.5
       (envelop ins time)
       (+
        (* 0.1 (modulate #'osc-square (/ frequency 2) #'osc-sine 1.5 1.0 time))
        (* 0.9 (osc-random 0 0))))))
