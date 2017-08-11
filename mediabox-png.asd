(asdf:defsystem #:mediabox-png
  :description "A reader and writer for the PNG image format."
  :author ("Michael Fiano <michael.fiano@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mediabox-png"
  :bug-tracker "https://github.com/mfiano/mediabox-png/issues"
  :source-control (:git "git@github.com:mfiano/mediabox-png.git")
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
   (:file "utils")
   (:file "conditions")
   (:file "chunk")
   (:file "chunk-data")
   (:file "datastream")
   (:file "deinterlace")
   (:file "decode")
   (:file "png")))
