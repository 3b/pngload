(in-package :mediabox-png)

(define-condition png-error (error)
  ((png-data :reader png-data
             :initarg :png-data)))

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (format s "Stream does not contain a valid PNG datastream: ~A."
                     (get-data-path (png-data c))))))
