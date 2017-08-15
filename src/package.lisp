(in-package :cl-user)

(defpackage #:pngload
  (:use #:cl
        #:alexandria
        #:fast-io)
  (:export #:load-file
           #:load-stream
           #:write-file
           #:write-stream))
