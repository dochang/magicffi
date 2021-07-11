
(in-package :magicffi)

(defun pathname-file (file &rest flags
                 &key debug symlink compress devices continue check preserve-atime raw error
                   compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                   no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Returns a description, as in unix `file` command"
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (cons :none flags))
    (magic-file m file)))

(defun pathname-mime-type (file &rest flags
                  &key debug symlink compress devices continue check preserve-atime raw error
                    compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                    no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Return the MIME type"
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (cons :mime-type flags))
    (magic-file m file)))

(defun pathname-mime-encoding (file &rest flags
                      &key debug symlink compress devices continue check preserve-atime raw error
                        compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                        no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Return the MIME encoding"
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (cons :mime-encoding flags))
    (magic-file m file)))

(defun pathname-mime (file &rest flags
                  &key debug symlink compress devices continue check preserve-atime raw error
                    compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                    no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Return the MIME type"
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (list* :mime-type :mime-encoding flags))
    (magic-file m file)))

(defun pathname-apple (file &rest flags
              &key debug symlink compress devices continue check preserve-atime raw error
                compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Return the Apple creator/type"
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (cons :apple flags))
    (magic-file m file)))

(defun pathname-extension (file &rest flags
                  &key debug symlink compress devices continue check preserve-atime raw error
                    compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                    no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding)
  "Return a /-separated list of extensions. This is NOT about the file name extensions (such as .png)."
  (declare (ignorable debug symlink compress devices continue check preserve-atime raw error
                      compress-transp no-check-compress no-check-tar no-check-soft no-check-apptype
                      no-check-elf no-check-text no-check-cdf no-check-tokens no-check-encoding))
  (with-open-magic (m (cons :extension flags))
    (magic-file m file)))

