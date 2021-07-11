MAGICFFI [![Build Status](https://travis-ci.org/guicho271828/magicffi.svg?branch=master)](https://travis-ci.org/guicho271828/magicffi)
========

MAGICFFI is a CFFI interface to libmagic(3), the file type
determination library using ``magic'' numbers.

Download
--------

To check out the git repository, run:

```
$ git clone git://github.com/guicho271828/magicffi.git
```

Installation
------------

MAGICFFI depends on the following lisp libraries:

- CFFI
- CL-PPCRE (used by the internal flags generator)

The following system packages are also required:

- libmagic-dev
- libc6-dev
- gcc

You have to install them on your system before loading magicffi.

Tutorial
--------

``` common-lisp
> (asdf:load-system :magicffi)
> (use-package :magicffi)
> (with-open-magic (magic '(:mime-type :symlink))
    (magic-file magic #P"magicffi.asd"))
"text/plain"
```

Documentation
-------------

All external symbols of the package `MAGICFFI` are listed in
`package.lisp`.  Check out their docstrings for help.

Magic Flags
-----------

You can use either constants or keyword list as magic flags.  I.e.,

``` common-lisp
(magic-open (logxor +magic-mime-type+ +magic-symlink+))
```


is equivalent to

``` common-lisp
(magic-open '(:mime-type :symlink))
```

The constant name and keyword name of a flag are based on its C name.
See the following table for the pattern:

| C name       | Lisp constant  | Lisp keyword |
|--------------|----------------|--------------|
| `MAGIC_NONE` | `+MAGIC-NONE+` | `:NONE`      |


All constant names are exported.

Credit
-------

+ Desmond O. Chang (original author)
+ Peter Hillerström
+ Pocket7878
+ Masataro Asai
