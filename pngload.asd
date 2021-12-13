(asdf:defsystem #:pngload
  :description "A reader for the PNG image format."
  :author ("Michael Fiano <mail@mfiano.net>"
           "Bart Botta <00003b@gmail.com>")
  :license "MIT"
  :homepage "https://github.com/mfiano/pngload"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:3bz
               #:alexandria
               (:feature (:and (:not :mezzano) (:not :abcl)) #:cffi)
               (:feature (:and (:not :mezzano) (:not :abcl)) #:mmap)
               #:parse-float
               (:feature (:and (:not :clisp) (:not :abcl)) #:static-vectors)
               #:swap-bytes
               #:uiop
               #:zpb-exif)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "source")
   (:file "source-noffi" :if-feature (:or :mezzano :abcl))
   (:file "source-ffi" :if-feature (:and (:not :mezzano) (:not :abcl)))
   (:file "properties")
   (:file "chunk")
   (:file "chunk-types")
   (:file "conditions")
   (:file "datastream")
   (:file "deinterlace")
   (:file "decode")
   (:file "metadata")
   (:file "png")
   (:file "octet-vector")
   (:file "png-nommap" :if-feature (:or :mezzano :abcl))
   (:file "png-mmap" :if-feature (:and (:not :mezzano) (:not :abcl)))))
