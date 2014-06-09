
(asdf:defsystem #:scheduler
  :name "scheduler"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "The Time Based Task Scheduler"
  :version "0.1.5"
  :depends-on (#:bordeaux-threads #:bt-semaphore #:pileup #:cffi)
  :serial t
  :components ((:file "package") 
	       #+windows (:file "threads-windows")
	       #-windows (:file "threads-posix")
	       (:file "time")
	       (:file "scheduler")
	       (:file "sync-tool"))) 
