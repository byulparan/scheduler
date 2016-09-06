(in-package #:scheduler)

(defstruct node timestamp task)

(defclass scheduler ()
  ((name
    :initarg :name
    :initform nil
    :reader sched-name)
   (mutex
    :reader mutex)
   (condition-var
    :initform #-ecl (bt:make-condition-variable)
	      #+ecl (bt-sem:make-semaphore)
    :reader condition-var)
   (in-queue
    :initform (pileup:make-heap #'<= :size 100 :key #'node-timestamp)
    :reader in-queue)
   (sched-thread
    :initform nil
    :accessor sched-thread)
   (status
    :initform :stop
    :accessor sched-status)
   (ahead
    :initarg :sched-ahead
    :initform .3
    :accessor sched-ahead)
   (timestamp
    :initarg :timestamp
    :initform #'unix-time
    :reader timestamp
    :documentation
    "This Function is get current scheduler time. That must based on seconds.")))

(defmethod initialize-instance :after ((self scheduler) &key)
  ;;; pilep:heap include lock. so scheduler use that lock.
  (with-slots (mutex in-queue) self
    #-ecl (setf mutex (slot-value in-queue 'pileup::lock))
    #+ecl (setf mutex (bt:make-recursive-lock))))


;;; timed wait -----------------------------------------------------------------------------------------

(defun condition-wait (condition-variable lock)
  #-ecl (bt:condition-wait condition-variable lock)
  #+ecl
  (progn
    (bt:release-lock lock)
    (unwind-protect (bt-sem:wait-on-semaphore condition-variable)
      (bt:acquire-lock lock t))))

(defun condition-timed-wait (condition-variable lock time)
  #+sbcl (unless (sb-thread:condition-wait condition-variable lock :timeout time)
	   (bt:acquire-lock lock t))
  #-sbcl
  (progn
    (bt:release-lock lock)
    (unwind-protect
	 #+ccl (ccl:timed-wait-on-semaphore condition-variable time)
      #+ecl(bt-sem:wait-on-semaphore condition-variable :timeout time)
      (bt:acquire-lock lock t))))

;;; -----------------------------------------------------------------------------------------------------

(defun sched-time (scheduler)
  (funcall (timestamp scheduler)))

(defun sched-quant (scheduler quantized-time &optional (offset-time 0.0d0))
  "Return a time which quantized to given a quantized-time."
  (let ((time (+ offset-time (sched-time scheduler))))
    (+ time (- quantized-time (mod time quantized-time)))))

(defun sched-run (scheduler)
  (when (eql (sched-status scheduler) :stop)
    (setf (sched-thread scheduler)
	  (bt:make-thread
	   (lambda ()
	     (labels ((run ()
			(handler-case
			    (loop
			      (loop :while (pileup:heap-empty-p (in-queue scheduler))
				    :do (condition-wait (condition-var scheduler) (mutex scheduler)))
			      (loop :while (not (pileup:heap-empty-p (in-queue scheduler)))
				    :do (let ((timeout (- (node-timestamp (pileup:heap-top (in-queue scheduler))) (sched-time scheduler))))
					  (unless (plusp timeout) (return))
					  (condition-timed-wait (condition-var scheduler) (mutex scheduler) timeout)))
			      (loop :while (and (not (pileup:heap-empty-p (in-queue scheduler)))
						(>= (sched-time scheduler) (node-timestamp (pileup:heap-top (in-queue scheduler)))))
				    :do (funcall (node-task (pileup:heap-pop (in-queue scheduler))))))
			  (error (c) (format t "~&Error \"~a\" in scheduler thread~%" c)
			    (run)))))
	       (set-thread-realtime-priority) ;thread-boost!!
	       (bt:with-lock-held ((mutex scheduler))
		 (setf (sched-status scheduler) :running)
		 (sched-clear scheduler)
		 (run))))
	   :name (format nil "~@[~a ~]scheduler thread" (sched-name scheduler))))
    :running))

(defun sched-add (scheduler time f &rest args)
  "Insert task and time-info to scheduler queue. scheduler have ahead of time value(default to 0.3).
 '(- time (sched-ahead scheduler)) is actual time it runs to f."
  (bt:with-recursive-lock-held ((mutex scheduler))
    (pileup:heap-insert (make-node :timestamp (- time (sched-ahead scheduler))
				   :task (lambda () (apply f args)))
			(in-queue scheduler))
    #-ecl (bt:condition-notify (condition-var scheduler))
    #+ecl (bt-sem:signal-semaphore (condition-var scheduler)))
  (values))

(defun sched-clear (scheduler)
  "Clear to scheduler queue."
  (bt:with-recursive-lock-held ((mutex scheduler))
    (let ((queue (in-queue scheduler)))
      (loop :while (not (pileup:heap-empty-p queue))
	    :do (pileup:heap-pop queue)))
    #-ecl (bt:condition-notify (condition-var scheduler))
    #+ecl (bt-sem:signal-semaphore (condition-var scheduler)))
  (values))


(defun sched-stop (scheduler)
  "Stop the scheduler."
  (when (eql (sched-status scheduler) :running)
    (bt:destroy-thread (sched-thread scheduler))
    (setf (sched-status scheduler) :stop)))



;;; functions for main scheduler ---------------------------------------------------------------------
;;; Scheduler library have *main-scheduler*. This API are functions for only *main-scheduler*.
;;; Unless you needs multiple scheduler, use this API.

;; (defparameter *main-scheduler* (make-instance 'scheduler :name "main"))

;; (defparameter *scheduling-mode* :realtime)

;; (defun callback (time f &rest args)
;;   (ecase *scheduling-mode*
;;     (:realtime (sched-add *main-scheduler* time (lambda () (apply f args))) )
;;     (:step (apply f args))))

;; (defun scheduler-running-p ()
;;   (eql (sched-status *main-scheduler*) :running))

;; (defun scheduler-start ()
;;   (sched-run *main-scheduler*))

;; (defun scheduler-clear ()
;;   (sched-clear *main-scheduler*))

;; (defun scheduler-stop ()
;;   (sched-stop *main-scheduler*))

;; (defun now ()
;;   (sched-time *main-scheduler*))

