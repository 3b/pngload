(in-package #:pngload)

(define-condition png-condition ()
  ((%chunk :reader chunk
           :initarg :chunk)
   (%path :reader path
          :initarg :path)))

(define-condition png-warning (png-condition warning) ())

(define-condition png-error (png-condition error) ())

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (format s "Not a valid PNG datastream: ~a." (path c)))))

(define-condition unknown-chunk-detected (png-warning) ()
  (:report (lambda (c s)
             (multiple-value-bind (type bytes) (get-chunk-type (chunk c))
               (format s "Unknown chunk type: ~s ~s in stream: ~a."
                       type
                       bytes
                       (path c))))))

(define-condition chunk-not-implemented (png-warning) ()
  (:report (lambda (c s)
             (multiple-value-bind (type bytes) (get-chunk-type (chunk c))
               (format s "Extension chunk type not implemented: ~s ~s ~
                          in stream: ~a."
                       type
                       bytes
                       (path c))))))
