(asdf:defsystem #:pngload-fast.test
  :description "Tests for pngload-fast."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Bart Botta <00003b@gmail.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pngload-fast"
  :bug-tracker "https://github.com/HackerTheory/pngload-fast/issues"
  :source-control (:git "https://github.com/HackerTheory/pngload-fast.git")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:opticl
               #:png-read
               #:pngload-fast)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
