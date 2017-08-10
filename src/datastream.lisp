(in-package :mediabox-png)

(defclass datastream ()
  ((signature :reader signature)
   (chunks :accessor chunks :initform nil)))

(defgeneric parse (parse-data node &key &allow-other-keys))

(defmethod parse (parse-data (node (eql :datastream)) &key)
  (let ((node (make-instance 'datastream)))
    (with-slots (datastream) parse-data
      (with-slots (signature chunks) node
        (setf signature (parse parse-data :datastream/signature)
              chunks (parse parse-data :datastream/chunks))))
    node))

(defmethod parse (parse-data (node (eql :datastream/signature)) &key)
  (let ((signature (read-bytes (buffer parse-data) :count 8)))
    (if (octets= signature '(137 80 78 71 13 10 26 10))
        signature
        (error 'invalid-png-stream :parse-data parse-data))))

(defmethod parse (parse-data (node (eql :datastream/chunks)) &key)
  (loop :with last-chunk
        :until (eq last-chunk :iend)
        :for chunk = (parse parse-data :chunk)
        :do (setf last-chunk (chunk-type-name (chunk-type chunk)))
        :collect chunk))
