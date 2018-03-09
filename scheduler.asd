(asdf:defsystem #:scheduler
  :name "scheduler"
  :author "Park Sungmin. byulparan@gmail.com"
  :description "The Time Based Musical Event Scheduler"
  :license "Apache-2.0"
  :depends-on (#:bordeaux-threads #:pileup #:cffi #+ecl #:bt-semaphore)
  :serial t
  :components ((:file "package") 
	       (:file #-(or win32 windows mswindows) "threads-posix"
		      #+(or win32 windows mswindows) "threads-windows")
	       (:file "time")
	       (:file "scheduler")))
