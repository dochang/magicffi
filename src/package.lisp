(in-package :cl-user)

(defpackage :magicffi
  (:use :cl :cffi)
  (:documentation "Usage:
> (asdf:load-system :magicffi)
> (use-package :magicffi)
> (with-open-magic (magic '(:mime-type :symlink))
    (magic-load magic)
    (magic-file magic #P\"magicffi.asd\"))
\"text/plain\"
")
  (:export
   ;; types
   :magic

   ;; special variables
   :*magic-database*

   ;; functions
   :magicp
   :open-magic-p
   :magic-open
   :magic-close
   :magic-file
   :magic-buffer
   :magic-setflags
   :magic-check
   :magic-compile
   :magic-load

   ;; conditions
   :magic-error
   :magic-error-errno
   :magic-error-error

   ;; flag constants
   :+MAGIC-NONE+
   :+MAGIC-DEBUG+
   :+MAGIC-SYMLINK+
   :+MAGIC-COMPRESS+
   :+MAGIC-DEVICES+
   :+MAGIC-MIME-TYPE+
   :+MAGIC-CONTINUE+
   :+MAGIC-CHECK+
   :+MAGIC-PRESERVE-ATIME+
   :+MAGIC-RAW+
   :+MAGIC-ERROR+
   :+MAGIC-MIME-ENCODING+
   :+MAGIC-MIME+
   :+MAGIC-APPLE+
   :+MAGIC-NO-CHECK-COMPRESS+
   :+MAGIC-NO-CHECK-TAR+
   :+MAGIC-NO-CHECK-SOFT+
   :+MAGIC-NO-CHECK-APPTYPE+
   :+MAGIC-NO-CHECK-ELF+
   :+MAGIC-NO-CHECK-TEXT+
   :+MAGIC-NO-CHECK-CDF+
   :+MAGIC-NO-CHECK-TOKENS+
   :+MAGIC-NO-CHECK-ENCODING+
   :+MAGIC-NO-CHECK-ASCII+
   :+MAGIC-NO-CHECK-FORTRAN+
   :+MAGIC-NO-CHECK-TROFF+

   ;; macros
   :with-open-magic

   ;; more api
   :pathname-extension
   :pathname-apple
   :pathname-mime
   :pathname-mime-encoding
   :pathname-mime-type
   :pathname-file))
