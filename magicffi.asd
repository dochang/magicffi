
(defsystem magicffi
  :description "cffi interface to libmagic(3)"
  :long-description "A file type determination library."
  :author "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :maintainer "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :license "Simplified BSD License"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi-grovel :cffi :cl-ppcre)
  :serial t
  :pathname "src"
  :components ((:file :package)
               (:cffi-grovel-file :grovel
                                  :cc-flags
                                  (#+linux "-idirafter"
                                           ;; because /usr/include/linux
                                           ;; contains an empty stddef.h which
                                           ;; overrides the correct stddef.h .
                                           #+linux "/usr/include/linux"
                                           ;; FIXME: It is here on ubuntu 16.04.
                                           ;; Verify this on other OSes
                                   "-I/usr/include"
                                   "-I/usr/local/include")
                                  )
               (:file :types)
               (:file :api)
               (:file :shortcuts)))

(defsystem magicffi/test
  :description "magicffi test system"
  :long-description "Test system for magicffi, a file type determination library."
  :author "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :maintainer "Desmond O. Chang <dochang+magicffi@gmail.com>"
  :license "Simplified BSD License"
  :depends-on (:magicffi :fiveam)
  :pathname "tests"
  :serial t
  :components ((:file :package))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :magicffi)"))))
