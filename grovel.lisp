(cl:in-package :magicffi)

(include-or "/usr/include/magic.h"
            "/usr/local/include/magic.h")

(progn
 (constant (+magic-none+ "MAGIC_NONE") :documentation "No flags")
 (constant (+magic-debug+ "MAGIC_DEBUG") :documentation "Turn on debugging")
 (constant (+magic-symlink+ "MAGIC_SYMLINK") :documentation "Follow symlinks")
 (constant (+magic-compress+ "MAGIC_COMPRESS") :documentation
  "Check inside compressed files")
 (constant (+magic-devices+ "MAGIC_DEVICES") :documentation
  "Look at the contents of devices")
 (constant (+magic-mime-type+ "MAGIC_MIME_TYPE") :documentation
  "Return the MIME type")
 (constant (+magic-continue+ "MAGIC_CONTINUE") :documentation
  "Return all matches")
 (constant (+magic-check+ "MAGIC_CHECK") :documentation
  "Print warnings to stderr")
 (constant (+magic-preserve-atime+ "MAGIC_PRESERVE_ATIME") :documentation
  "Restore access time on exit")
 (constant (+magic-raw+ "MAGIC_RAW") :documentation
  "Don't convert unprintable chars")
 (constant (+magic-error+ "MAGIC_ERROR") :documentation
  "Handle ENOENT etc as real errors")
 (constant (+magic-mime-encoding+ "MAGIC_MIME_ENCODING") :documentation
  "Return the MIME encoding")
 (constant (+magic-mime+ "MAGIC_MIME") :documentation nil)
 (constant (+magic-apple+ "MAGIC_APPLE") :documentation
  "Return the Apple creator/type")
 (constant (+magic-extension+ "MAGIC_EXTENSION") :documentation
  "Return a /-separated list of extensions")
 (constant (+magic-compress-transp+ "MAGIC_COMPRESS_TRANSP") :documentation
  "Check inside compressed files but not report compression")
 (constant (+magic-nodesc+ "MAGIC_NODESC") :documentation nil)
 (constant (+magic-no-check-compress+ "MAGIC_NO_CHECK_COMPRESS") :documentation
  "Don't check for compressed files")
 (constant (+magic-no-check-tar+ "MAGIC_NO_CHECK_TAR") :documentation
  "Don't check for tar files")
 (constant (+magic-no-check-soft+ "MAGIC_NO_CHECK_SOFT") :documentation
  "Don't check magic entries")
 (constant (+magic-no-check-apptype+ "MAGIC_NO_CHECK_APPTYPE") :documentation
  "Don't check application type")
 (constant (+magic-no-check-elf+ "MAGIC_NO_CHECK_ELF") :documentation
  "Don't check for elf details")
 (constant (+magic-no-check-text+ "MAGIC_NO_CHECK_TEXT") :documentation
  "Don't check for text files")
 (constant (+magic-no-check-cdf+ "MAGIC_NO_CHECK_CDF") :documentation
  "Don't check for cdf files")
 (constant (+magic-no-check-tokens+ "MAGIC_NO_CHECK_TOKENS") :documentation
  "Don't check tokens")
 (constant (+magic-no-check-encoding+ "MAGIC_NO_CHECK_ENCODING") :documentation
  "Don't check text encodings")
 (constant (+magic-no-check-builtin+ "MAGIC_NO_CHECK_BUILTIN") :documentation
  nil)
 (constant (+magic-no-check-ascii+ "MAGIC_NO_CHECK_ASCII") :documentation nil)
 (constant (+magic-no-check-fortran+ "MAGIC_NO_CHECK_FORTRAN") :documentation
  "Don't check ascii/fortran")
 (constant (+magic-no-check-troff+ "MAGIC_NO_CHECK_TROFF") :documentation
  "Don't check ascii/troff")
 (constant (+magic-version+ "MAGIC_VERSION") :documentation
  "This implementation")
 (constant (+magic-param-indir-max+ "MAGIC_PARAM_INDIR_MAX") :documentation
  nil)
 (constant (+magic-param-name-max+ "MAGIC_PARAM_NAME_MAX") :documentation nil)
 (constant (+magic-param-elf-phnum-max+ "MAGIC_PARAM_ELF_PHNUM_MAX")
  :documentation nil)
 (constant (+magic-param-elf-shnum-max+ "MAGIC_PARAM_ELF_SHNUM_MAX")
  :documentation nil)
 (constant (+magic-param-elf-notes-max+ "MAGIC_PARAM_ELF_NOTES_MAX")
  :documentation nil)
 (constant (+magic-param-regex-max+ "MAGIC_PARAM_REGEX_MAX") :documentation
  nil)
 (constant (+magic-param-bytes-max+ "MAGIC_PARAM_BYTES_MAX") :documentation
  nil))

(bitfield magic-flags ((:none "MAGIC_NONE") :documentation "No flags")
 ((:debug "MAGIC_DEBUG") :documentation "Turn on debugging")
 ((:symlink "MAGIC_SYMLINK") :documentation "Follow symlinks")
 ((:compress "MAGIC_COMPRESS") :documentation "Check inside compressed files")
 ((:devices "MAGIC_DEVICES") :documentation "Look at the contents of devices")
 ((:mime-type "MAGIC_MIME_TYPE") :documentation "Return the MIME type")
 ((:continue "MAGIC_CONTINUE") :documentation "Return all matches")
 ((:check "MAGIC_CHECK") :documentation "Print warnings to stderr")
 ((:preserve-atime "MAGIC_PRESERVE_ATIME") :documentation
  "Restore access time on exit")
 ((:raw "MAGIC_RAW") :documentation "Don't convert unprintable chars")
 ((:error "MAGIC_ERROR") :documentation "Handle ENOENT etc as real errors")
 ((:mime-encoding "MAGIC_MIME_ENCODING") :documentation
  "Return the MIME encoding")
 ((:mime "MAGIC_MIME") :documentation nil)
 ((:apple "MAGIC_APPLE") :documentation "Return the Apple creator/type")
 ((:extension "MAGIC_EXTENSION") :documentation
  "Return a /-separated list of extensions")
 ((:compress-transp "MAGIC_COMPRESS_TRANSP") :documentation
  "Check inside compressed files but not report compression")
 ((:nodesc "MAGIC_NODESC") :documentation nil)
 ((:no-check-compress "MAGIC_NO_CHECK_COMPRESS") :documentation
  "Don't check for compressed files")
 ((:no-check-tar "MAGIC_NO_CHECK_TAR") :documentation
  "Don't check for tar files")
 ((:no-check-soft "MAGIC_NO_CHECK_SOFT") :documentation
  "Don't check magic entries")
 ((:no-check-apptype "MAGIC_NO_CHECK_APPTYPE") :documentation
  "Don't check application type")
 ((:no-check-elf "MAGIC_NO_CHECK_ELF") :documentation
  "Don't check for elf details")
 ((:no-check-text "MAGIC_NO_CHECK_TEXT") :documentation
  "Don't check for text files")
 ((:no-check-cdf "MAGIC_NO_CHECK_CDF") :documentation
  "Don't check for cdf files")
 ((:no-check-tokens "MAGIC_NO_CHECK_TOKENS") :documentation
  "Don't check tokens")
 ((:no-check-encoding "MAGIC_NO_CHECK_ENCODING") :documentation
  "Don't check text encodings")
 ((:no-check-builtin "MAGIC_NO_CHECK_BUILTIN") :documentation nil)
 ((:no-check-ascii "MAGIC_NO_CHECK_ASCII") :documentation nil)
 ((:no-check-fortran "MAGIC_NO_CHECK_FORTRAN") :documentation
  "Don't check ascii/fortran")
 ((:no-check-troff "MAGIC_NO_CHECK_TROFF") :documentation
  "Don't check ascii/troff")
 ((:version "MAGIC_VERSION") :documentation "This implementation")
 ((:param-indir-max "MAGIC_PARAM_INDIR_MAX") :documentation nil)
 ((:param-name-max "MAGIC_PARAM_NAME_MAX") :documentation nil)
 ((:param-elf-phnum-max "MAGIC_PARAM_ELF_PHNUM_MAX") :documentation nil)
 ((:param-elf-shnum-max "MAGIC_PARAM_ELF_SHNUM_MAX") :documentation nil)
 ((:param-elf-notes-max "MAGIC_PARAM_ELF_NOTES_MAX") :documentation nil)
 ((:param-regex-max "MAGIC_PARAM_REGEX_MAX") :documentation nil)
 ((:param-bytes-max "MAGIC_PARAM_BYTES_MAX") :documentation nil))
