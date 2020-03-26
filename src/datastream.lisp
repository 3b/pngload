(in-package #:pngload)

(defstruct datastream
  signature
  chunks)

(defun parse-datastream ()
  (let ((datastream (make-datastream)))
    (setf (datastream-signature datastream) (parse-signature)
          (datastream-chunks datastream) (parse-all-chunks))
    (when *decode-data*
      (decode))
    datastream))

(defun parse-signature ()
  (with-source (*png-source*)
    (let ((signature (loop repeat 8 collect (ub8))))
      (if (equalp signature '(137 80 78 71 13 10 26 10))
          signature
          (error 'invalid-png-stream)))))

(defun parse-all-chunks ()
  (loop :for chunk = (parse-chunk)
        :collect chunk
        :when (eq (chunk-name chunk) :iend)
          :do (loop-finish)))
