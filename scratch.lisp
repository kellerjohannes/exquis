(in-package :exquis)

(defparameter *keys* (make-array 62 :initial-element '(:interval 0)))

(defun get-freq (key-id)
  ())

(defconstant +num-moogs+ 5)

(defparameter *moog-status* (make-array +num-moogs+ :initial-element nil))

(defun get-next-free-moog ()
  (dotimes (i +num-moogs+)
    (unless (aref *moog-status* i)
      (return i))))

(defparameter *selected-moog* nil)


(vug:dsp! moog (freq amp vcf res)
  (incudine.util:with-samples ((in (* amp (vug:moogladder (vug:phasor freq 0) vcf res))))
    (vug:out in in)))

(defun start-moogs ()
  (dotimes (i +num-moogs+)
    (moog 440 0.5 100 1 :id (1+ i))))

(defun stop-moogs ()
  (dotimes (i +num-moogs+)
    (incudine:free (1+ i))))

(defparameter *global-amp* 0.15)

(defun play-moog (freq)
  (let ((id (get-next-free-moog)))
    (incudine:set-control (1+ id) :amp *global-amp*)
    (incudine:set-control (1+ id) :freq freq)
    (setf (aref *moog-status* id) t)))

(defun stop-moog (moog-id)
  (incudine:set-control (1+ moog-id) :amp 0)
  (setf (aref *moog-status* moog-id) nil))


;; (incf (incudine:control-value 1 :amp) 0.1)

;; (incudine:set-control 1 :freq 70)
;; (incudine:set-control 1 :vcf 280)
;; (incudine:set-control 1 :res 0.2)
;; (incudine:set-control 1 :amp 0.9)


;; (incudine:free 1)


(defparameter *exquis-out* (jackmidi:open :port-name "Exquis out" :direction :output))

(defparameter *exquis-in* (jackmidi:open :port-name "Exquis in"))

(defparameter *time-delta* 4000)

(defparameter *color* (list 100 0 0))

(defparameter *running* t)

(defparameter *tuningp* nil)

(defun start-tuning ()
  (setf *tuningp* t)
  (set-color 100 127 0 0))

(defun stop-tuning ()
  (setf *tuningp* nil)
  (setf *selected-moog* nil)
  (set-color 100 10 10 10))

(defun exquis-callbacks (status data1 data2)
  (format t "~&~d:~d:~d~%" status data1 data2)
  (case status
    (159 (when (< data1 61)
           (if *tuningp*
               (progn
                 (setf *selected-moog* data2)
                 (play-moog (get-freq data1)))
               (if (= data2 127)
                   (play-moog (get-freq data1))
                   (stop-moog (get-moog-id data1))))))
    ;; (176 (case data1
    ;;        (41 (setf *time-delta* (* (1+ data2) 100)))
    ;;        (21 (pads-off))
    ;;        (42 (setf (first *color*) data2))
    ;;        (43 (setf (second *color*) data2))
    ;;        (44 (setf (third *color*) data2))))
    (191 (case data1
           ;; (113 (incf (incudine:control-value 1 :amp) (* 0.01 (- data2 64)))
           ;;  (format t "~&amp: ~d" (incudine:control-value 1 :amp)))
           ;; (112 (incf (incudine:control-value 1 :vcf) (* 2 (- data2 64)))
           ;;  (format t "~&vcf: ~d" (incudine:control-value 1 :vcf)))
           ;; (111 (incf (incudine:control-value 1 :res) (* 0.01 (- data2 64)))
           ;;  (format t "~&res: ~d" (incudine:control-value 1 :res)))
           ;; (110 (incf (incudine:control-value 1 :freq) (* 0.1 (- data2 64)))
           ;;  (format t "~&freq: ~d" (incudine:control-value 1 :freq)))
           (100 (when (eq data2 127)
                  (format t "~&Tuning mode.~%")
                  (if *tuningp*
                      (stop-tuning)
                      (start-tuning)
                      )))))))

(defun init ()
  (incudine:rt-start)
  (incudine:remove-all-responders *exquis-in*)
  (incudine:make-responder *exquis-in* (lambda (status data1 data2)
                                         (funcall #'exquis-callbacks status data1 data2)))
  (incudine:recv-start *exquis-in*))


(defparameter *lookup-masks* `((:pads . #x01)
                               (:encoders . #x02)
                               (:slider . #x04)
                               (:up-down . #x08)
                               (:settings . #x10)
                               (:other . #x20)
                               (:exit . #x00)
                               (:all . ,(+ #x01 #x02 #x04 #x08 #x10 #x20))))

(defun lookup-mask (mask-keyword)
  (let ((result (cdr (assoc mask-keyword *lookup-masks*))))
    (if result result #x00)))

(defun developer-mode (mask)
  (let ((msg (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents `(#xf0 #x00 #x21 #x7e #x7f #x00 ,(lookup-mask mask)
                                                    #xf7))))
    (jackmidi:write *exquis-out* msg)))

(defun set-color (key-id r g b)
  (let ((msg (make-array 12 :element-type '(unsigned-byte 8)
                           :initial-contents `(#xf0 #x00 #x21 #x7e #x7f #x04
                                                    ,key-id
                                                    ,r ,g ,b #x00
                                                    #xf7))))
    (jackmidi:write *exquis-out* msg)))


(defun swipe (key-id)
  (when *running*
    (cond ((< key-id 0) (swipe 0))
          ((>= key-id 61) (swipe 0))
          (t (set-color key-id (first *color*) (second *color*) (third *color*))
             (incudine:at (+ (incudine:now) 4000) #'swipe (1+ key-id))))))


(defun pads-off ()
  (dotimes (i 61)
    (set-color i 0 0 0)))

(defun random-color ()
  (when *running*
    (set-color (random 61) (random 127) (random 127) (random 127))
    (incudine:at (+ (incudine:now) *time-delta*) #'random-color)))

(defun defined-color ()
  (when *running*
    (set-color (random 61) (first *color*) (second *color*) (third *color*))
    (incudine:at (+ (incudine:now) *time-delta*) #'defined-color)))
