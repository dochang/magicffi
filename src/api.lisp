(in-package :magicffi)

;; check if groveller successfully groveled

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (boundp '+magic-version+))
    (error "grovelling +magic-version+ from MAGIC_VERSION in magic.h header file failed!
 Make sure your system has the header file installed.
 For example, in ubuntu/debian, try `sudo apt get install libmagic-dev`.")))

(define-foreign-library libmagic
  (:unix (:or "libmagic.so.1.0.0" "libmagic.so.1" "libmagic.so"))
  (t (:default "libmagic")))

(use-foreign-library libmagic)

(defun magic-error (magic)
  "Signals an error of type MAGIC-ERROR."
  (error 'magic-error
         :errno (foreign-funcall "magic_errno" cmagic magic :int)
         :error (foreign-funcall "magic_error" cmagic magic :string)))

(defcvar *errno* :int "A symbol macro for accessing C ERRNO static variable.")

(defun magic-open (flags)
  "Creates a magic cookie and returns it.  An error of type
SIMPLE-ERROR is signaled on failure.  FLAGS specifies how the other
magic functions should behave.  See README for the flags usage."
  (or (foreign-funcall "magic_open" magic-flags flags cmagic)
      (error "Error(~A): ~A"
             *errno*
             (foreign-funcall "strerror" :int *errno* :string))))

(defun magic-close (magic)
  "Closes the magic database and deallocates any resources used.  It
is permissible to close an already closed magic, and has no effect.
Returns 'true' if an open magic cookie has been closed, or 'false' if
the magic cookie is already closed."
  (when (open-magic-p magic)
    (foreign-funcall "magic_close"
                     cmagic magic
                     :void)
    (setf (%magic-cookie magic) nil)
    t))

(defun %truename (filespec)
  (namestring (truename filespec)))

(defun magic-file (magic pathspec)
  "Returns a textual description of the contents of the PATHSPEC
argument.  PATHSPEC is a pathname designator.  An error of type
MAGIC-ERROR is signaled on failure."
  (or (foreign-funcall "magic_file"
                       cmagic magic
                       :string (%truename pathspec)
                       :string)
      (magic-error magic)))

(defun magic-buffer (magic string)
  "Returns a textual description of the contents of the STRING argument.
An error of type MAGIC-ERROR is signaled on failure."
  (let ((size (length string)))
    (with-foreign-string (buffer string)
      (or (foreign-funcall "magic_buffer"
                           cmagic magic
                           :pointer buffer
                           size size
                           :string)
          (magic-error magic)))))

(defun magic-setflags (magic flags)
  "Sets the magic flags.  Signals an error of type SIMPLE-ERROR on
systems that don't support utime(2), or utimes(2) when :PRESERVE-ATIME
is set; otherwise, returns 'true'."
  (or (foreign-funcall "magic_setflags"
                       cmagic magic
                       magic-flags flags
                       magic-boolean)
      (error "~A" "Sets the :PRESERVE-ATIME flag on a system which doesn't support utime(2), or utimes(2).")))

(defun %pathname-concat (seq)
  (flet ((%concat (&optional a b)
           (and a b (concatenate 'string a ":" b))))
    (reduce #'%concat seq :key '%truename)))

(defun %pathlist-to-cstring (pathname-list)
  (typecase pathname-list
    (null (null-pointer))
    (atom (%truename pathname-list))
    (cons (%pathname-concat pathname-list))))

(defvar *magic-database* nil
  "Default magic database files.  It can be NIL(default), or a
designator for a non-empty list of pathname designators.  NIL means
the default database files defined by libmagic.")

(defmacro %database-funcall (name-and-options magic pathname-list)
  `(or (let* ((*magic-database* (or ,pathname-list *magic-database*)))
         (foreign-funcall ,name-and-options
                          cmagic ,magic
                          :string (%pathlist-to-cstring *magic-database*)
                          magic-boolean))
       (magic-error ,magic)))

(defun magic-check (magic &optional pathname-list)
  "Checks the validity of database files.  PATHNAME-LIST is
NIL(default), which means use \*MAGIC-DATABASE*, or a designator for a
non-empty list of pathname designators.  Returns 'true' on success and
signals an error of type MAGIC-ERROR on failure."
  (%database-funcall "magic_check" magic pathname-list))

(defun magic-compile (magic &optional pathname-list)
  "Compiles database files.  PATHNAME-LIST is NIL(default), which
means use \*MAGIC-DATABASE*, or a designator for a non-empty list of
pathname designators.  Returns 'true' on success and signals an error
of type MAGIC-ERROR on failure.  The compiled files created are named
from the basename(1) of each file argument with `.mgc' appended to
it."
  (%database-funcall "magic_compile" magic pathname-list))

(defun magic-load (magic &optional pathname-list)
  "Loads database files.  PATHNAME-LIST is NIL(default), which means
use \*MAGIC-DATABASE*, or a designator for a non-empty list of
pathname designators.  Returns 'true' on success and signals an error
of type MAGIC-ERROR on failure."
  (%database-funcall "magic_load" magic pathname-list))

(defmacro with-open-magic ((magic &optional (flags ''(:none)) (magicfiles nil)) &body body)
  "Opens the magic cookie MAGIC, executes BODY and close MAGIC.

FLAGS:
 A list of keywords (see types), defaulted to (:none).

MAGICFILES:
 NIL, or a list of pathname designators for the database files.
 Defaulted to NIL, which uses the default database available in the system. This covers the most usage.
"
  `(progn
     (magic-verify-version)
     (let* ((,magic (magic-open ,flags)))
       (unwind-protect
            (progn
              (magic-load ,magic ,magicfiles)
              ,@body)
         (magic-close ,magic)))))

(defun magic-version ()
  "The magic_version() command returns the version number of this library
which is compiled into the shared library using the constant
MAGIC_VERSION from <magic.h>.  This can be used by client programs to
verify that the version they compile against is the same as the version
that they run against."
  (foreign-funcall "magic_version" :int))

(defun magic-verify-version ()
  (assert
   (=
    ;; version in the shared library
    (magic-version)
    ;; version in the header
    +magic-version+)))
