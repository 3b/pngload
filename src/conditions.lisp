(in-package :mediabox-png)

(define-condition png-warning (warning)
  ((parse-data :reader parse-data
               :initarg :parse-data)))

(define-condition png-error (error)
  ((parse-data :reader parse-data
               :initarg :parse-data)))

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (format s "Stream does not contain a valid PNG datastream: ~A."
                     (get-data-path (parse-data c))))))

(define-condition unknown-chunk-detected (png-warning) ()
  (:report (lambda (c s)
             (format s "Detected an unknown chunk type in PNG datastream: ~A."
                     (get-data-path (parse-data c))))))

(define-condition draft-chunk-detected (png-warning)
  ((chunk-type :reader chunk-type
               :initarg :chunk-type))
  (:report (lambda (c s)
             (format s "Detected a draft chunk type (~S) that has not yet been ~
approved by the PNG developers, therefor will be skipped: ~A."
                     (chunk-type c)
                     (get-data-path (parse-data c))))))
