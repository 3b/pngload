(in-package #:pngload)

(defstruct datastream
  signature
  chunks)

(defun parse-datastream ()
  (let ((datastream (make-datastream)))
    (setf (datastream-signature datastream) (parse-signature)
          (datastream-chunks datastream) (parse-all-chunks))
    (when (state-decode-data (state *png*))
      (decode))
    datastream))

(defun parse-signature ()
  (with-source ((state-source (state *png*)))
    (let ((signature (loop :repeat 8 :collect (ub8))))
      (if (equalp signature '(137 80 78 71 13 10 26 10))
          signature
          (error 'invalid-png-stream)))))

(defun parse-all-chunks ()
  (loop :for chunk = (parse-chunk)
        :collect chunk
        :when (= (chunk-type chunk) #x49454e44)
          :do (loop-finish)))
