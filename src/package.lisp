(in-package :cl-user)

(defpackage #:pngload
  (:use #:cl
        #:alexandria
        #:fast-io)
  (:export #:load-file
           #:load-stream
           #:write-file
           #:write-stream
           #:width
           #:height
           #:bit-depth
           #:color-type
           #:palette-count
           #:palette
           #:transparency
           #:interlace-method
           #:data))
