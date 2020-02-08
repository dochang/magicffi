(in-package :cl-user)

(defpackage :magicffi/test
  (:use :cl :magicffi :fiveam))


(in-package :magicffi/test)


(def-suite :magicffi)
(in-suite :magicffi)

(defun rel (name &optional (dir #p"tests/"))
  (asdf:system-relative-pathname :magicffi (merge-pathnames name dir)))

(test magicffi
  (is (equal "PDF document, version 1.5"
             (with-open-magic (magic)
               (magic-file magic (rel "test.pdf")))))
  (is (equal "SVG Scalable Vector Graphics image"
             (with-open-magic (magic)
               (magic-file magic (rel "test.svg")))))
  (is (equal "PNG image data, 1050 x 1485, 8-bit/color RGBA, non-interlaced"
             (with-open-magic (magic)
               (magic-file magic (rel "test.png")))))
  (is (equal "Lisp/Scheme program, ASCII text"
             (with-open-magic (magic)
               (magic-file magic (rel "package.lisp")))))

  (is (equal "PDF document, version 1.5"
             (with-open-magic (magic '(:none))
               (magic-file magic (rel "test.pdf")))))
  (is (equal "SVG Scalable Vector Graphics image"
             (with-open-magic (magic '(:none))
               (magic-file magic (rel "test.svg")))))
  (is (equal "PNG image data, 1050 x 1485, 8-bit/color RGBA, non-interlaced"
             (with-open-magic (magic '(:none))
               (magic-file magic (rel "test.png")))))
  (is (equal "Lisp/Scheme program, ASCII text"
             (with-open-magic (magic '(:none))
               (magic-file magic (rel "package.lisp")))))
  
  (is (equal "application/pdf; charset=binary"
             (with-open-magic (magic '(:mime))
               (magic-file magic (rel "test.pdf")))))
  (is (equal "image/svg+xml; charset=utf-8"
             (with-open-magic (magic '(:mime))
               (magic-file magic (rel "test.svg")))))
  (is (equal "image/png; charset=binary"
             (with-open-magic (magic '(:mime))
               (magic-file magic (rel "test.png")))))
  (is (equal "text/x-lisp; charset=us-ascii"
             (with-open-magic (magic '(:mime))
               (magic-file magic (rel "package.lisp")))))


  (is (equal "utf-8"
             (pathname-mime-encoding (rel "testtxt.utf8"))))
  (is (equal "us-ascii"
             (pathname-mime-encoding (rel "testtxt.ascii"))))
  (is (equal "iso-8859-1"
             (pathname-mime-encoding (rel "testtxt.eucjp")))))
