(in-package #:scheduler)

#+(and darwin (not ccl))
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
 
#+ecl
(progn
  (cffi:defctype time_t :long)
  (cffi:defctype seconds_t :int)

  (cffi:defcstruct timeval
    (tv_sec time_t)
    (tv_usec seconds_t))

  (cffi:defcfun gettimeofday :int
    (timeval :pointer)
    (pointer :pointer))

  (defun unix-time ()
    (cffi:with-foreign-object (tv '(:struct timeval))
      (gettimeofday tv (cffi::null-pointer))
      (+ (cffi:mem-ref tv 'time_t) (* (cffi:mem-ref tv 'seconds_t (cffi:foreign-type-size 'time_t)) 1.0d-6)))))


(defun now ()
  #+(and darwin ccl) (* 1.0d-9 (ccl:current-time-in-nanoseconds))
  #+(and darwin (not ccl)) (* 1.0d-9 (cffi:foreign-funcall "AudioGetCurrentHostTime" :unsigned-long-long))
  #-darwin (unix-time))
