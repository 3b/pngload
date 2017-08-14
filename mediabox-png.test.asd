(asdf:defsystem #:mediabox-png.test
  :description "Tests for mediabox-png."
  :author ("Michael Fiano <michael.fiano@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mediabox-png"
  :bug-tracker "https://github.com/mfiano/mediabox-png/issues"
  :source-control (:git "git@github.com:mfiano/mediabox-png.git")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:mediabox-png
               #:png-read
               #:png
               #:sdl2-image
               #:cl-soil)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
