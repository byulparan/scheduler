(in-package #:scheduler)

(defparameter *timer-function* #'(lambda ()
				   #+ccl (* 1.0d-9 (ccl:current-time-in-nanoseconds))
				   #+sbcl (multiple-value-bind (secs usecs)
					      (sb-ext:get-time-of-day)
					    (+ secs (* usecs 1.0d-6)))))

(defun now ()
  (funcall *timer-function*))

(defun set-timer-func (func)
  (when (scheduler-running-p)
    (scheduler-clear))
  (setf *timer-function* func))


