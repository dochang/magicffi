(in-package :magicffi)

;;; cffi
(defctype size :unsigned-int)

;;; errors
(define-condition magic-error (error)
  ((%errno :initarg :errno
           :reader magic-error-errno)
   (%error :initarg :error
           :reader magic-error-error))
  (:report %magic-error-reporter)
  (:documentation "Consists of errors that are related to MAGICFFI.
Use the function MAGIC-ERROR to signal it.  The error number and error
string are accessed by the functions MAGIC-ERROR-ERRNO and
MAGIC-ERROR-ERROR."))

(defun %magic-error-reporter (condition stream)
  (format stream "Error(~A): ~A"
          (magic-error-errno condition)
          (magic-error-error condition)))

;;; magic-boolean
(defun %boolean-to-lisp (value)
  (zerop value))

(defun %boolean-from-lisp (value)
  (if value 0 -1))

(defctype magic-boolean
    (:wrapper :int
              :from-c %boolean-to-lisp
              :to-c %boolean-from-lisp)
  "0 is 'true', -1 is 'false'.")

;;; cmagic
(defclass magic ()
  ((cookie :initarg :cookie
           :accessor %magic-cookie))
  (:documentation "Lisp magic class."))

(defun magicp (object)
  "Returns 'true' if object is of type MAGIC; otherwise, returns 'false'.
It is unaffected by whether object, if it is a magic, is open or
closed."
  (typep object 'magic))

(defun open-magic-p (magic)
  "Returns 'true' if MAGIC is open; otherwise, returns 'false'."
  (if (magicp magic)
      (and (%magic-cookie magic)
           (not (null-pointer-p (%magic-cookie magic))))
      (error 'type-error :datum magic :expected-type 'magic)))

(defun %magic-c-to-lisp (cookie)
  (if (null-pointer-p cookie)
      nil
      (make-instance 'magic :cookie cookie)))

(defun %magic-lisp-to-c (magic)
  (if (open-magic-p magic)
      (%magic-cookie magic)
      (error "The magic cookie is closed.")))

(defctype cmagic
    (:wrapper :pointer
              :from-c %magic-c-to-lisp
              :to-c %magic-lisp-to-c)
  "The magic cookie")
