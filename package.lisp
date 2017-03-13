
(defpackage #:scheduler
  (:use #:cl)
  (:export #:get-thread-priority
	   #:set-thread-realtime-priority

	   #:unix-time
	   
	   #:scheduler
	   #:sched-time
	   #:sched-quant
	   #:sched-run
	   #:sched-add
	   #:sched-clear
	   #:sched-stop
	   #:sched-status
	   #:sched-ahead))
