(in-package :pngload)

(defmacro with-octet-pointer-source ((source &key end) &body body)
  (declare (ignore source end body))
  `(error "can't load from pointers on this platform"))


(defmacro with-source ((source &key buffer end) &body body)
  `(etypecase ,source
     (octet-vector-source
      (with-octet-vector-source (,source :end ,end) ,@body))
     (octet-pointer-source
      (with-octet-pointer-source (,source :end ,end) ,@body))
     (file-stream-source
      (with-file-stream-source (,source :buffer ,buffer :end ,end) ,@body))
     (stream-source
      (with-stream-source (,source :buffer ,buffer :end ,end) ,@body))))
