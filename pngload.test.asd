(asdf:defsystem #:pngload.test
  :description "Tests for pngload."
  :author ("Michael Fiano <mail@mfiano.net>"
           "Bart Botta <00003b@gmail.com>")
  :license "MIT"
  :homepage "https://github.com/bufferswap/pngload"
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
