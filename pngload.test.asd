(asdf:defsystem #:pngload.test
  :description "Tests for pngload."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Bart Botta <00003b@gmail.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/pngload"
  :bug-tracker "https://github.com/mfiano/pngload/issues"
  :source-control (:git "git@github.com:mfiano/pngload.git")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:opticl
               #:png-read
               #:pngload)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
