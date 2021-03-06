(in-package :cl-user)

(defpackage :magicffi/asdf
  (:use :cl :asdf))

(in-package :magicffi/asdf)

(eval-when (:load-toplevel :execute)
  (load-system :cffi-grovel))

(defsystem :magicffi
  :description "cffi interface to libmagic(3)"
  :long-description "A file type determination library."
  :author "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :maintainer "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :license "Simplified BSD License"
  :depends-on (:cffi :cl-ppcre)
  :serial t
  :components ((:file :package)
               (:file :flags-generator)
               (:file :cffi-grovel-patch)
               (cffi-grovel:grovel-file :grovel)
               (:file :types)
               (:file :api)))

(defsystem :magicffi/test
  :description "magicffi test system"
  :long-description "Test system for magicffi, a file type determination library."
  :author "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :maintainer "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :license "Simplified BSD License"
  :depends-on (:magicffi :alexandria)
  :components ((:module "tests"
                :serial t
                :components ((:file :package)
                             (:file :tests)))))

(defmethod perform ((o test-op) (c (eql (find-system :magicffi))))
  (load-system :magicffi/test)
  (funcall (intern "RUN-TESTS" :magicffi/test)))
