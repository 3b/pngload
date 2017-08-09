(in-package :mediabox-png)

(defclass datastream ()
  ((signature :reader signature)
   (chunks :accessor chunks :initform nil)))

(defgeneric parse (png-data node &key &allow-other-keys))

(defmethod parse (png-data (node (eql :datastream)) &key)
  (let ((node (make-instance 'datastream)))
    (with-slots (datastream) png-data
      (with-slots (signature chunks) node
        (setf signature (parse png-data :datastream/signature)
              chunks (parse png-data :datastream/chunks))))
    node))

(defmethod parse (png-data (node (eql :datastream/signature)) &key)
  (let ((signature (buffer-data png-data :bytes 8)))
    (if (octets= signature '(137 80 78 71 13 10 26 10))
        signature
        (error 'invalid-png-stream :png-data png-data))))

(defmethod parse (png-data (node (eql :datastream/chunks)) &key)
  (loop :with last-chunk-p
        :until last-chunk-p
        :for chunk = (parse png-data :chunk)
        :for type = (chunk-type-name (chunk-type chunk))
        :when (eq type :iend)
          :do (setf last-chunk-p t)
        :collect chunk))
