(asdf:defsystem #:pngload
  :description "A reader for the PNG image format."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Bart Botta <00003b@gmail.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pngload"
  :source-control (:git "git@github.com:mfiano/pngload.git")
  :bug-tracker "https://github.com/mfiano/pngload/issues"
  :version "1.0.1"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:parsley)
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
