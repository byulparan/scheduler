# Scheduler
**The time based task scheduler for Common Lisp**

#### version: 2017.3.14

#### require:
  - [Quicklisp](http://www.quicklisp.org)
  - [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org) or [ECL](http://ecls.sourceforge.net)

#### timestamp:

   Scheduler's timestamp using UNIX-time(since 1970.1.1) on all platform.

#### usage:

	(in-package :scheduler)
	
	(defvar *scheduler* (make-instance 'scheduler :ahead ..)) ;make scheduler object
	
	(sched-run *scheduler*) ;start scheduler
	
	(sched-add *scheduler* (+ 4 (sched-time *scheduler*)) #'task-function args...) ;insert task to scheduer queue with time
	
	(sched-clear *scheduler*) ;clear to scheduler queue
	
	(sched-stop *scheduler*) ;stop scheduler

##### Remember! scheduler have 'ahead time'.
what is 'ahead time'? please read this document(<http://impromptu.moso.com.au/tutorials/time.html>).  
summary

	 If you are executing a call to evaluate a note (now) by the time the code is evaluated it will already be late.
	 You should always try to schedule your code execution ahead of the scheduled time of your tasks.

default 'ahead time' is 0.3 seconds. so this codes are execute after 3.7 seconds.

	(sched-add *scheduler* (+ 4 (sched-time *scheduler*)) #'task-function args...)

You can get/set to ahead value of scheduler object by **#'ahead** accessor. 


