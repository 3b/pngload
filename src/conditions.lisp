(in-package :mediabox-png)

(define-condition png-warning (warning) ())

(define-condition png-error (error) ())

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Stream does not contain a valid PNG datastream: ~A."
                     (get-path)))))

(define-condition unknown-chunk-detected (png-warning) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Detected an unknown chunk type in PNG datastream: ~A."
                     (get-path)))))

(define-condition draft-chunk-detected (png-warning)
  ((chunk-type :reader chunk-type
               :initarg :chunk-type))
  (:report (lambda (c s)
             (format s "Detected a draft chunk type (~S) that has not yet been ~
approved by the PNG developers, therefor will be skipped: ~A."
                     (chunk-type c)
                     (get-path)))))
