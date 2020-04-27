(in-package #:pngload)

(define-condition png-warning (warning) ())

(define-condition png-error (error) ())

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Stream does not contain a valid PNG datastream: ~a."
                     (get-path)))))

(define-condition unknown-chunk-detected (png-warning)
  ((%chunk ::reader chunk
           :initarg :chunk))
  (:report (lambda (c s)
             (let* ((type-bytes (loop :with type = (chunk-type (chunk c))
                                      :for i :below 32 :by 8
                                      :collect (ldb (byte 8 i) type)
                                        :into result
                                      :finally (return (nreverse result))))
                    (type-str (map 'string 'code-char type-bytes)))
               (format s "Unknown chunk type ~s = ~s in PNG datastream: ~a."
                       (if (every 'graphic-char-p type-str)
                           type-str
                           (coerce type-str 'list))
                       type-bytes
                       (get-path))))))
