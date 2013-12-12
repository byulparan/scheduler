(in-package #:scheduler)

(defun now ()
  #+ccl (* 1.0d-9 (ccl:current-time-in-nanoseconds))
  #+sbcl (multiple-value-bind (secs usecs)
	     (sb-ext:get-time-of-day)
	   (+ secs (* usecs 1.0d-6))))

(defstruct node :timestamp :task)

(defclass scheduler ()
  ((mutex :reader mutex)
   (condition-var :initform (bt:make-condition-variable) :reader condition-var)
   (in-queue :initform (pileup:make-heap #'< :size 100 :key #'node-timestamp) :reader in-queue)
   (sched-thread :initform nil :accessor sched-thread)
   (status :initform :stop :accessor status)
   (ahead :initarg :ahead :initform .3 :accessor ahead)))

(defmethod initialize-instance :after ((self scheduler) &key)
  ;;; pilep:heap include lock. so scheduler use that lock.
  (with-slots (mutex in-queue) self
    (setf mutex (slot-value in-queue 'pileup::lock))))


;;; timed wait -----------------------------------------------------------------------------------------
;;; bordeaux-threads not support timed wait for condition variable.

#+ccl
(defun condition-timed-wait (condition-variable lock time)
  (when (plusp time)
    (bt:release-lock lock)
    (prog1 (ccl:timed-wait-on-semaphore condition-variable time)
      (bt:acquire-lock lock))))

#+sbcl
(defun condition-timed-wait (condition-variable lock time)
  (let ((result (sb-thread:condition-wait condition-variable lock :timeout time)))
    (unless result (sb-thread:grab-mutex lock))
    result))

;;; -----------------------------------------------------------------------------------------------------

(defvar *in-sched* nil
  "in scheduler thread, not need grab mutex, notify signal.
 so this variable used for check to current thread == scheduler thread.")

(defun sched-run (scheduler)
  (when (eql (status scheduler) :running)
    (error "scheduler already running"))
  (setf (sched-thread scheduler)
	(bt:make-thread
	 (lambda ()
	   (labels ((run ()
		      (handler-case
			  (loop
			    (loop :while (pileup:heap-empty-p (in-queue scheduler))
				  :do (bt:condition-wait (condition-var scheduler) (mutex scheduler)))
			    (loop :while (not (pileup:heap-empty-p (in-queue scheduler)))
				  :do (let ((timestamp (node-timestamp (pileup:heap-top (in-queue scheduler)))))
					(when (>= 0.001 (- timestamp (now))) (return)) 
					(condition-timed-wait (condition-var scheduler) (mutex scheduler) (- timestamp (now)))))
			    (loop :while (and (not (pileup:heap-empty-p (in-queue scheduler)))
					      (>= (now) (node-timestamp (pileup:heap-top (in-queue scheduler)))))
				  :do (funcall (node-task (pileup:heap-pop (in-queue scheduler))))))
			(error (c) (format t "~&Error \"~a\" in scheduler thread~%" c)
			  (run)))))
	     (set-real-time-thread-priority) 	;thread-boost!!
	     (bt:with-lock-held ((mutex scheduler))
	       (let ((*in-sched* t))
		 (setf (status scheduler) :running)
		 (sched-clear scheduler)
		 (run)))))
	 :name "scheduler thread"))
  (values))


(defmacro with-condition-lock ((scheduler) &body body)
  `(if *in-sched* (progn ,@body)
       (bt:with-lock-held ((mutex ,scheduler))
	 ,@body
	 (bt:condition-notify (condition-var ,scheduler)))))

(defun sched-add (scheduler time f &rest args)
  "Insert task and time-info to scheduler queue. scheduler have ahead of time value(default to 0.3).
 '(- time (ahead scheduler)) is actual time it runs to f."
  (with-condition-lock (scheduler)
    (pileup:heap-insert (make-node :timestamp (- time (ahead scheduler))
				   :task (lambda () (apply f args)))
			(in-queue scheduler)))
  (values))

(defun sched-clear (scheduler)
  "Clear to scheduler queue."
  (with-condition-lock (scheduler)
    (let ((queue (in-queue scheduler)))
      (loop :while (not (pileup:heap-empty-p queue))
	    :do (pileup:heap-pop queue))))
  (values))

(defun sched-stop (scheduler)
  "Stop the scheduler."
  (with-slots (sched-thread status) scheduler
    (when (eql status :stop)
      (error "scheduler not running"))
    (bt:destroy-thread sched-thread)
    (setf status :stop)))



;;; functions for main scheduler ---------------------------------------------------------------------
;;; Scheduler library have *main-scheduler*. This API are functions for only *main-scheduler*.
;;; Unless you needs multiple scheduler, use this API.

(defparameter *main-scheduler* (make-instance 'scheduler))
(defparameter *scheduling-mode* :realtime)

(defun callback (time f &rest args)
  (ecase *scheduling-mode*
    (:realtime (sched-add *main-scheduler* time (lambda () (apply f args))) )
    (:step (apply f args))))

(defun scheduler-running-p ()
  (eql (status *main-scheduler*) :running))

(defun scheduler-start ()
  (sched-run *main-scheduler*))

(defun scheduler-clear ()
  (sched-clear *main-scheduler*))

(defun scheduler-stop ()
  (sched-stop *main-scheduler*))
