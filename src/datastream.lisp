(in-package :mediabox-png)

(defclass datastream ()
  ((signature :reader signature)
   (chunks :accessor chunks :initform nil)))

(defun parse-datastream ()
  (let ((node (make-instance 'datastream)))
    (with-slots (signature chunks) node
      (setf signature (parse-signature)
            chunks (parse-all-chunks)))
    (decode)
    node))

(defun parse-signature ()
  (let ((signature (read-bytes 8)))
    (if (octets= signature '(137 80 78 71 13 10 26 10))
        signature
        (error 'invalid-png-stream))))

(defun parse-all-chunks ()
  (loop :for chunk = (parse-chunk)
        :collect chunk
        :when (eq (chunk-name chunk) :iend)
          :do (loop-finish)))
