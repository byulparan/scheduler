
(asdf:defsystem #:scheduler
  :name "scheduler"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "The Time Based Task Scheduler"
  :version "0.1"
  :depends-on (#:bordeaux-threads #:pileup #-windows #:cffi)
  :serial t
  :components ((:file "package") 
	       #+windows (:file "threads-windows")
	       #-windows (:file "threads-posix")
	       (:file "scheduler")
	       (:file "sync"))) 
