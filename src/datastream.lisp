(in-package :mediabox-png)

(defclass datastream ()
  ((signature :reader signature)
   (chunks :accessor chunks :initform nil)))

(defun parse-datastream (parse-data)
  (let ((node (make-instance 'datastream)))
    (with-slots (signature chunks) node
      (setf signature (parse-signature parse-data)
            chunks (parse-all-chunks parse-data)))
    node))

(defun parse-signature (parse-data)
  (let ((signature (read-bytes (buffer parse-data) :count 8)))
    (if (octets= signature '(137 80 78 71 13 10 26 10))
        signature
        (error 'invalid-png-stream :parse-data parse-data))))

(defun parse-all-chunks (parse-data)
  (loop :with last-chunk
        :until (eq last-chunk :iend)
        :for chunk = (parse-chunk parse-data)
        :do (setf last-chunk (chunk-type-name (chunk-type chunk)))
        :collect chunk))
