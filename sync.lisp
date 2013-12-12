(in-package :scheduler)

;;; -------------------------------------------------------------------
;;; sync tool.

(defclass sync-tool ()
  ((offset :initform nil :accessor offset)
   (sync-thread :accessor sync-thread))
  (:documentation "Each Application, Library have unique timestamp. sync-tool object used
 synchronize between #'now and other timestamp."))

(defun make-sync-tool (base-time-function &optional (thread-name "time-sync thread"))
  "Creates a sync-tool which synchronized to base-time-function. base-time-function must based on seconds."
  (let ((sync-tool (make-instance 'sync-tool)))
    (setf (sync-thread sync-tool)
	  (bt:make-thread
	   (lambda ()
	     (loop 
	       (let* ((min-diff #x7fffFFFFffffFFFF))
		 (dotimes (i 8)
		   (let* ((before (now))
			  (base (funcall base-time-function))
			  (after (now))
			  (diff (- after before)))
		     (when (< diff min-diff)
		       (setf min-diff diff)
		       (setf (offset sync-tool) (- base (+ before (* diff .5))))))))
	       (sleep 20)))
	   :name thread-name))
    sync-tool))

(defun destroy-sync-tool (sync-tool)
  (bt:destroy-thread (sync-thread sync-tool))
  (setf (offset sync-tool) nil))

;;; ---------------------------------------------------------------------
;;; Utility for time_sync.

(defun quant (next-time)
  "Return a time which quantized to given a next-time."
  (let ((time (* 1.0d0 (floor (now)))))
    (+ time (- next-time (mod time next-time)))))
