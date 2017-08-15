(in-package :cl-user)

(defpackage #:mediabox-png
  (:use #:cl
        #:alexandria
        #:fast-io)
  (:export #:load-file
           #:load-stream
           #:write-file
           #:write-stream))
