;;; Scheduler require accurate timing. so I search how to increase priority of scheduler-thread.
;;; This implements is generally posix thread's way.
;;; but' I don't know that have effect, actually...

(in-package :scheduler)

(cffi:defcstruct sched-param
  (priority :int))

#+darwin
(cffi:defcenum sched-policy
  (:sched_other 1)
  (:sched_rr 2)
  (:sched_fifo 4))

#+linux
(cffi:defcenum sched-policy
  (:sched_other 0)
  (:sched_fifo 1)
  (:sched_rr 2))


(defun set-thread-realtime-priority ()
  "This function is made high priority to calling thread, and sched-policy set SCHED_RR."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param))))
    (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
      (setf priority 76))
    (cffi:foreign-funcall "pthread_setschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :int (cffi:foreign-enum-value 'sched-policy :sched_rr)
						  :pointer param)))


(defun get-thread-priority ()
  "Get the thread-info of calling thread. If you want get thread-info of *main-scheduler*,
 eval the '(callback (now) #'get-thread-priority)."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param)))
			      (policy :int))
    (cffi:foreign-funcall "pthread_getschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :pointer policy
						  :pointer param)
    (format t "~&policy: ~d~%priority: ~d" (let ((policy (cffi:mem-ref policy :int)))
					     (cffi:foreign-enum-keyword 'sched-policy policy))
	    (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
	      priority))))





