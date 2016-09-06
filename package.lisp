
(defpackage #:scheduler
  (:nicknames :cb)
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
	   #:sched-ahead
	   
	   ;; #:*main-scheduler*
	   ;; #:callback
	   ;; #:scheduler-running-p
	   ;; #:scheduler-start
	   ;; #:scheduler-clear
	   ;; #:scheduler-stop
	   ;; #:now
	   
	   ;; #:make-sync-tool
	   ;; #:destroy-sync-tool
	   ;; #:offset


	   ))
