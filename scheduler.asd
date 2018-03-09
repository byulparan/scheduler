(asdf:defsystem #:scheduler
  :name "scheduler"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "The Time Based Task Scheduler"
  :version "2017.3.14"
  :depends-on (#:bordeaux-threads #:pileup #:cffi #+ecl #:bt-semaphore)
  :serial t
  :components ((:file "package") 
	       (:file #-(or win32 windows mswindows) "threads-posix"
		      #+(or win32 windows mswindows) "threads-windows")
	       (:file "time")
	       (:file "scheduler")))
