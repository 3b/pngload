(in-package #:pngload)

(define-condition png-condition ()
  ((%png :reader png
         :initarg :png)
   (%chunk :reader chunk
           :initarg :chunk)))

(define-condition png-warning (png-condition warning) ())

(define-condition png-error (png-condition error) ())

(define-condition file-not-found (png-error) ()
  (:report (lambda (c s)
             (format s "File not found: ~a," (path (png c))))))

(define-condition file-too-small (png-error) ()
  (:report (lambda (c s)
             (format s "File to small to be a PNG file: ~a," (path (png c))))))

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (format s "Not a valid PNG datastream: ~a." (path (png c))))))

(define-condition unknown-chunk-detected (png-warning) ()
  (:report (lambda (c s)
             (multiple-value-bind (type bytes) (get-chunk-type (chunk c))
               (format s "Unknown chunk type: ~s ~s in stream: ~a."
                       type
                       bytes
                       (path (png c)))))))

(define-condition chunk-not-implemented (png-warning) ()
  (:report (lambda (c s)
             (multiple-value-bind (type bytes) (get-chunk-type (chunk c))
               (format s "Extension chunk type not implemented: ~s ~s ~
                          in stream: ~a."
                       type
                       bytes
                       (path (png c)))))))
