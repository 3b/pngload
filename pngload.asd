(asdf:defsystem #:pngload
  :description "A reader for the PNG image format."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Bart Botta <00003b@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/pngload"
  :bug-tracker "https://github.com/mfiano/pngload/issues"
  :source-control (:git "git@github.com:mfiano/pngload.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:fast-io
               #:babel
               #:chipz)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "properties")
   (:file "conditions")
   (:file "chunk")
   (:file "chunk-data")
   (:file "datastream")
   (:file "deinterlace")
   (:file "decode")
   (:file "png")))
