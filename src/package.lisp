(in-package :cl-user)

(defpackage #:mediabox-png
  (:use #:cl
        #:alexandria
        #:fast-io)
  (:export #:read-file
           #:read-datastream
           #:write-file
           #:write-datastream))
