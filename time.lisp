(in-package #:scheduler)

#+(and darwin sbcl)
(progn
  (cffi:define-foreign-library core-audio
    (:darwin (:framework "CoreAudio")))
  (cffi:use-foreign-library core-audio))

#+sbcl
(defun unix-time ()
  (multiple-value-bind (secs usecs)
      (sb-ext:get-time-of-day)
    (+ secs (* usecs 1.0d-6))))

#+ccl
(defun unix-time ()
  (ccl:rlet ((tv :timeval))
    (ccl::gettimeofday tv)
    (multiple-value-bind (secs usecs)
	(values (ccl:pref tv :timeval.tv_sec) (ccl:pref tv :timeval.tv_usec))
      (+ secs (* usecs 1.0d-6)))))

(defun now ()
  #+(and ccl darwin) (* 1.0d-9 (ccl:current-time-in-nanoseconds))
  #+(and sbcl darwin) (* 1.0d-9 (cffi:foreign-funcall "AudioGetCurrentHostTime" :unsigned-long-long))
  #+linux (unix-time))
