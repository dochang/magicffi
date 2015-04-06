(in-package :cffi-grovel)

(define-grovel-syntax include-or (&rest file-names)
  (loop
     for file-name in file-names
     if (probe-file file-name)
     do
       (return
         (format out "#include <~A>~%" file-name))))

(push 'include-or *header-forms*)

;; Defines a bitfield, with elements specified as ((lisp-name c-name)
;; &key documentation).  name-and-opts can be either a symbol as name,
;; or a list (name &key base-type).
(define-grovel-syntax bitfield (name-and-opts &rest masks)
  (destructuring-bind (name &key base-type)
      (ensure-list name-and-opts)
    (c-section-header out "bitfield" name)
    (c-export out name)
    (c-format out "(cffi:defbitfield (")
    (c-print-symbol out name t)
    (when base-type
      (c-printf out " ")
      (c-print-symbol out base-type t))
    (c-format out ")")
    (dolist (mask masks)
      (destructuring-bind ((lisp-name c-name) &key documentation) mask
        (declare (ignore documentation))
        (check-type lisp-name symbol)
        (check-type c-name string)
        (c-format out "~%  (")
        (c-print-symbol out lisp-name)
        (c-format out " ")
        (c-printf out "%i" c-name)
        (c-format out ")")))
    (c-format out ")~%")))
