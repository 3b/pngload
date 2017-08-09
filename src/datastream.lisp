(in-package :mediabox-png)

(defclass datastream ()
  ((signature :reader signature)
   (chunks :accessor chunks :initform nil)))

(defgeneric parse (file node &key &allow-other-keys))

(defmethod parse (file (node (eql :datastream)) &key)
  (let ((node (make-instance 'datastream)))
    (with-slots (datastream) file
      (with-slots (signature) node
        (setf datastream node
              signature (parse file :datastream/signature))
        (parse file :datastream/chunks :tree node)))
    node))

(defmethod parse (file (node (eql :datastream/signature)) &key)
  (let ((signature (read-octets file :count 8)))
    (if (octets= signature '(137 80 78 71 13 10 26 10))
        signature
        (error 'invalid-file :file file))))

(defmethod parse (file (node (eql :datastream/chunks)) &key tree)
  (loop :with last-chunk-p
        :until last-chunk-p
        :for chunk = (parse file :chunk)
        :for type = (chunk-type->name chunk)
        :when (eq type :iend)
          :do (setf last-chunk-p t)
        :do (push chunk (chunks tree))
        :finally (nreversef (chunks tree))))
