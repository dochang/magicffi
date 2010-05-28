(in-package :cl-user)

(defpackage :magicffi-test
  (:use :cl :magicffi)
  (:import-from :alexandria :read-file-into-string)
  (:export :run-tests))
