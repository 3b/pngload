(asdf:defsystem #:pngload.test
  :description "Tests for pngload."
  :author ("Michael Fiano <mail@mfiano.net>"
           "Bart Botta <00003b@gmail.com>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/pngload"
  :bug-tracker "https://git.mfiano.net/mfiano/pngload/issues"
  :source-control (:git "https://git.mfiano.net/mfiano/pngload")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:opticl
               #:png-read
               #:pngload
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
