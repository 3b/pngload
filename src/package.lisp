(in-package #:cl-user)

(defpackage #:pngload
  (:use #:cl)
  (:export
   #:png
   #:load-file
   #:load-stream
   #:width
   #:height
   #:bit-depth
   #:color-type
   #:data
   #:get-metadata
   #:with-png-in-static-vector))
