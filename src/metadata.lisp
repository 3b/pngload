(in-package #:pngload)

#++(defun find-chunks (png chunk-type)
     (remove-if-not
      (lambda (x)
        (eq (alexandria:make-keyword (string-upcase (get-chunk-type x))) chunk-type))
      (datastream-chunks (parse-tree png))))
