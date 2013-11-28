
(defpackage #:scheduler
  (:nicknames :cb)
  (:use #:cl)
  (:export #:get-thread-priority

	   #:scheduler
	   #:sched-run
	   #:sched-add
	   #:sched-clear
	   #:sched-stop
	   #:status
	   #:ahead
	   
	   #:*main-scheduler*
	   #:callback
	   #:scheduler-running-p
	   #:scheduler-start
	   #:scheduler-clear
	   #:scheduler-stop

	   #:set-timer-func
	   #:now))
