(in-package :mediabox-png)

(define-condition png-error (error)
  ((file :reader file
         :initarg :file)))

(define-condition invalid-file (png-error) ()
  (:report (lambda (c s)
             (format s "File does not contain a valid PNG datastream: ~A."
                     (path (file c))))))
