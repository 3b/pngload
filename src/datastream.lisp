(in-package #:pngload)

(defstruct datastream
  signature
  chunks)

(defun parse-datastream (png)
  (let ((datastream (make-datastream)))
    (setf (datastream-signature datastream) (parse-signature png)
          (datastream-chunks datastream) (parse-all-chunks png))
    (when (state-decode-data (state png))
      (decode png))
    datastream))

(defun parse-signature (png)
  (with-source ((state-source (state png)))
    (let ((signature (loop :repeat 8 :collect (ub8))))
      (if (equalp signature '(137 80 78 71 13 10 26 10))
          signature
          (error 'invalid-png-stream :path (get-path png))))))

(defun parse-all-chunks (png)
  (loop :for chunk = (parse-chunk png)
        :collect chunk
        :when (= (chunk-type chunk) #x49454e44)
          :do (loop-finish)))
