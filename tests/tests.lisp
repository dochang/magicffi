(in-package :magicffi-test)

(defun do-test (testfile resultfile)
  (with-open-magic (cookie '(:none))
    (format t ";;; Loading ~A...~%" *magic-database*)
    (magic-load cookie)
    (format t ";;; Determining ~A...~%" testfile)
    (let* ((result (magic-file cookie testfile))
           (desired (read-file-into-string resultfile)))
      (format t "~A: ~A~%" testfile result)
      (unless (string= result desired)
        (error "Error: result was~%~A~%expected:~%~A~%" result desired)))))

(defun run-tests ()
  (let* ((this-file (or #.*compile-file-pathname*
                        (load-time-value *load-pathname*)))
         (testfiles (directory (make-pathname :name :wild
                                              :type "testfile"
                                              :defaults this-file))))
    (format t "~2&;;; Testing...~%")
    (dolist (testfile testfiles)
      (let* ((magicfile (make-pathname :type "magic" :defaults testfile))
             (resultfile (make-pathname :type "result" :defaults testfile))
             (*magic-database* magicfile))
        (do-test testfile resultfile)))
    (format t "~&;;; All tests passed!~%")))
