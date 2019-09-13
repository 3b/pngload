(in-package :pngload)

(defclass source ()
  ((start :reader start :initform 0 :initarg :start)
   (end :accessor end :initarg :end :initform nil)
   (pos :accessor pos :initform 0 :initarg :pos)
   (data :reader source-data :initarg :data)))

(defclass octet-vector-source (source)
  ())

(defclass octet-pointer-source (source)
  ())

(defclass file-stream-source (source)
  ())

(defclass stream-source (source)
  ())

(defmethod source-path (source)
  :in-memory)

(defmethod source-path ((source file-stream-source))
  (file-namestring (source-data source)))

(defun vector-source-from-stream (stream count)
  (let ((buf (make-array count :element-type 'ub8)))
    (read-sequence buf stream)
    (make-instance 'octet-vector-source :data buf :end count)))

(defmethod .source-region (source size &key &allow-other-keys)
  (let ((pos (pos source)))
    (assert (<= (+ pos size)
                (end source)))
    (make-instance (type-of source)
                   :data (source-data source)
                   :start pos
                   :end (+ pos size)
                   :pos pos)))

(defmethod .source-region ((source stream-source) size &key no-copy)
  (let ((pos (pos source))
        (data (source-data source)))
    (when (end source)
      (assert (<= (+ pos size) (end source))))
    (if no-copy
        (make-instance (type-of source)
                       :data data
                       :start pos
                       :end (+ pos size)
                       :pos pos)
        (vector-source-from-stream data size))))

(defmethod .source-region ((source file-stream-source) size &key no-copy)
  (let* ((data (source-data source))
         (pos (file-position data)))
    (prog1 (make-instance 'file-stream-source
                          :data data
                          :start pos
                          :end (+ pos size)
                          :pos pos)
      (unless no-copy
        (file-position data (+ pos size))))))

(defmacro with-octet-vector-source ((source &key end) &body body)
  (alexandria:with-gensyms (v p e e1)
    `(let ((,v (source-data ,source))
           (,p (pos ,source))
           ,@(when end `((,e1 (end ,source))))
           (,e ,(or end `(end ,source))))
       (declare (type ub8a1d ,v)
                (type fixnum ,p))
       (labels ((ub8 ()
                  (assert (< ,p ,e))
                  (prog1
                      (aref ,v ,p)
                    (incf ,p)))
                (ub16be ()
                  (assert (<= (+ ,p 2) ,e))
                  (prog1
                      (nibbles:ub16ref/be ,v ,p)
                    (incf ,p 2)))
                (ub32be ()
                  (assert (<= (+ ,p 4) ,e))
                  (prog1
                      (nibbles:ub32ref/be ,v ,p)
                    (incf ,p 4)))
                (chunk-offset ()
                  (- ,e ,p))
                (skip-bytes (count)
                  (assert (<= (+ ,p count) ,e))
                  (incf ,p count))
                (read-bytes (count &key zlib)
                  (let ((a (make-array count :element-type 'ub8)))
                    (map-into a #'ub8)
                    (when zlib
                      (setf a (3bz:decompress-vector a :format :zlib)))
                    a))
                (source-region (n)
                  (setf (pos ,source) ,p)
                  (prog1
                      (.source-region ,source n)
                    (setf ,p (+ ,p n))))
                (read-string (&key (bytes (- ,e ,p)) encoding
                                null-terminated-p zlib)
                  (let ((s (make-array bytes :element-type 'ub8)))
                    (loop for i below bytes
                          for b = (ub8)
                          do (setf (aref s i) b)
                          until (and null-terminated-p (zerop b)))
                    (when null-terminated-p
                      (setf s (subseq s 0 (position 0 s))))
                    (when zlib
                      (setf s (3bz:decompress-vector s :format :zlib)))
                    (babel:octets-to-string s :encoding encoding))))
         (declare (inline ub8 ub16be ub32be)
                  (ignorable #'ub8 #'ub16be #'ub32be
                             #'chunk-offset #'read-string
                             #'skip-bytes #'read-bytes
                             #'source-region))
         (unwind-protect
              (macrolet ((nest (&body b)
                           `(progn
                              (setf (pos ,',source) ,',p)
                              (progn ,@b)
                              (setf ,',p (pos ,',source)))))
                ,@(when end
                    `((assert (<= ,end (end ,source)))
                      (setf (end ,source) ,end)))
                (progn ,@body))
           (setf (pos ,source) ,p)
           ,@(when end
               `((setf (end ,source) ,e1))))))))

;; todo: VOPs for these?
(declaim (inline be16 be32))
(defun be16 (x)
  (declare (type ub16 x))
  #+little-endian
  (+ (ldb (byte 8 8) x)
     (ash (ldb (byte 8 0) x) 8))
  #+big-endian
  x)
(defun be32 (x)
  (declare (type ub32 x))
  #+little-endian
  (+ (ldb (byte 8 24) x)
     (ash (ldb (byte 8 16) x) 8)
     (ash (ldb (byte 8 8) x) 16)
     (ash (ldb (byte 8 0) x) 24))
  #+big-endian
  x)

(defmacro with-octet-pointer-source ((source &key end) &body body)
  (alexandria:with-gensyms (v p e e1)
    `(let ((,v (source-data ,source))
           (,p (pos ,source))
           ,@(when end `((,e1 (end ,source))))
           (,e ,(or end `(end ,source))))
       (declare (type cffi:foreign-pointer ,v)
                (type fixnum ,p))
       (labels ((ub8 ()
                  (assert (< ,p ,e))
                  (prog1
                      (cffi:mem-ref ,v :uint8 ,p)
                    (incf ,p)))
                (ub16be ()
                  (assert (<= (+ ,p 2) ,e))
                  (prog1
                      (be16 (cffi:mem-ref ,v :uint16 ,p))
                    (incf ,p 2)))
                (ub32be ()
                  (assert (<= (+ ,p 4) ,e))
                  (prog1
                      (be32 (cffi:mem-ref ,v :uint32 ,p))
                    (incf ,p 4)))
                (chunk-offset ()
                  (- ,e ,p))
                (skip-bytes (count)
                  (assert (<= (+ ,p count) ,e))
                  (incf ,p count))
                (read-bytes (count &key zlib)
                  (let ((a (make-array count :element-type 'ub8)))
                    (map-into a #'ub8)
                    (when zlib
                      (setf a (3bz:decompress-vector a :format :zlib)))
                    a))
                (source-region (n)
                  (setf (pos ,source) ,p)
                  (prog1
                      (.source-region ,source n)
                    (setf ,p (+ ,p n))))
                (read-string (&key (bytes (- ,e ,p)) encoding
                                null-terminated-p zlib)
                  (let ((s (make-array bytes :element-type 'ub8)))
                    (loop for i below bytes
                          for b = (ub8)
                          do (setf (aref s i) b)
                          until (and null-terminated-p (zerop b)))
                    (when null-terminated-p
                      (setf s (subseq s 0 (position 0 s))))
                    (when zlib
                      (setf s (3bz:decompress-vector s :format :zlib)))
                    (babel:octets-to-string s :encoding encoding))))
         (declare (inline ub8 ub16be ub32be)
                  (ignorable #'ub8 #'ub16be #'ub32be
                             #'chunk-offset #'read-string
                             #'skip-bytes #'read-bytes
                             #'source-region))
         (unwind-protect
              (macrolet ((nest (&body b)
                           `(progn
                              (setf (pos ,',source) ,',p)
                              (progn ,@b)
                              (setf ,',p (pos ,',source)))))
                ,@(when end
                    `((assert (<= ,end (end ,source)))
                      (setf (end ,source) ,end)))
                (progn ,@body))
           (setf (pos ,source) ,p)
           ,@(when end
               `((setf (end ,source) ,e1))))))))

(defmacro with-file-stream-source ((source &key buffer end) &body body)
  (if buffer
      `(let ((,source (vector-source-from-stream (source-data ,source)
                                                 ,buffer)))
         (with-octet-vector-source (,source)
           ,@body))
      (alexandria:with-gensyms (v p e e1)
        `(let ((,v (source-data ,source))
               (,p (pos ,source))
               ,@(when end `((,e1 (end ,source))))
               (,e ,(or end `(end ,source))))
           (declare (type file-stream ,v)
                    (type fixnum ,p))
           (labels ((ub8 ()
                      (assert (< ,p ,e))
                      (prog1
                          (read-byte ,v)
                        (incf ,p)))
                    (ub16be ()
                      (assert (<= (+ ,p 2) ,e))
                      (prog1
                          (nibbles:read-ub16/be ,v)
                        (incf ,p 2)))
                    (ub32be ()
                      (assert (<= (+ ,p 4) ,e))
                      (prog1
                          (nibbles:read-ub32/be ,v)
                        (incf ,p 4)))
                    (chunk-offset ()
                      (- ,e ,p))
                    (skip-bytes (count)
                      (assert (<= (+ ,p count) ,e))
                      (incf ,p count)
                      (file-position ,v ,p))
                    (read-bytes (count &key zlib)
                      (let ((a (make-array count :element-type 'ub8)))
                        (map-into a #'ub8)
                        (when zlib
                          (setf a (3bz:decompress-vector a :format :zlib)))
                        a))
                    (source-region (n)
                      (setf (pos ,source) ,p)
                      (prog1
                          (.source-region ,source n)
                        (setf ,p (+ ,p n))))
                    (read-string (&key (bytes (- ,e ,p)) encoding
                                    null-terminated-p zlib)
                      (let ((s (make-array bytes :element-type 'ub8)))
                        (loop for i below bytes
                              for b = (ub8)
                              do (setf (aref s i) b)
                              until (and null-terminated-p (zerop b)))
                        (when null-terminated-p
                          (setf s (subseq s 0 (position 0 s))))
                        (when zlib
                          (setf s (3bz:decompress-vector s :format :zlib)))
                        (babel:octets-to-string s :encoding encoding))))
             (declare (inline ub8 ub16be ub32be)
                      (ignorable #'ub8 #'ub16be #'ub32be
                                 #'chunk-offset #'read-string
                                 #'skip-bytes #'read-bytes
                                 #'source-region))
             (unwind-protect
                  (macrolet ((nest (&body b)
                               `(progn
                                  (setf (pos ,',source) ,',p)
                                  (progn ,@b)
                                  (setf ,',p (pos ,',source)))))
                    ,@(when end
                        `((assert (<= ,end (end ,source)))
                          (setf (end ,source) ,end)))
                    (progn ,@body))
               (setf (pos ,source) ,p)
               ,@(when end
                   `((setf (end ,source) ,e1)))))))))

(defmacro with-stream-source ((source &key buffer end) &body body)
  (if buffer
      `(let ((,source (vector-source-from-stream (source-data ,source)
                                                 ,buffer)))
         (with-octet-vector-source (,source)
           ,@body))
      (alexandria:with-gensyms (v p e e1)
        `(let ((,v (source-data ,source))
               (,p (pos ,source))
               ,@(when end `((,e1 (end ,source))))
               (,e ,(or end `(end ,source))))
           (declare (type file-stream ,v)
                    (type fixnum ,p))
           (labels ((ub8 ()
                      (when ,e (assert (< ,p ,e)))
                      (prog1
                          (read-byte ,v)
                        (incf ,p)))
                    (ub16be ()
                      (when ,e (assert (<= (+ ,p 2) ,e)))
                      (prog1
                          (nibbles:read-ub16/be ,v)
                        (incf ,p 2)))
                    (ub32be ()
                      (when ,e (assert (<= (+ ,p 4) ,e)))
                      (prog1
                          (nibbles:read-ub32/be ,v)
                        (incf ,p 4)))
                    (chunk-offset ()
                      (- ,e ,p))
                    (skip-bytes (count)
                      (loop repeat count do (ub8)))
                    (read-bytes (count &key zlib)
                      (let ((a (make-array count :element-type 'ub8)))
                        (map-into a #'ub8)
                        (when zlib
                          (setf a (3bz:decompress-vector a :format :zlib)))
                        a))
                    (source-region (n)
                      (setf (pos ,source) ,p)
                      (prog1
                          (.source-region ,source n)
                        (setf ,p (+ ,p n))))
                    (read-string (&key (bytes (- ,e ,p))
                                    encoding null-terminated-p
                                    zlib)
                      (let ((s (make-array bytes :element-type 'yub8)))
                        (loop for i below bytes
                              for b = (ub8)
                              do (setf (aref s i) b)
                              until (and null-terminated-p (zerop b)))
                        (when null-terminated-p
                          (setf s (subseq s 0 (position 0 s))))
                        (when zlib
                          (setf s (3bz:decompress-vector s :format :zlib)))
                        (babel:octets-to-string s :encoding encoding))))
             (declare (inline ub8 ub16be ub32be)
                      (ignorable #'ub8 #'ub16be #'ub32be
                                 #'chunk-offset #'read-string
                                 #'skip-bytes #'read-bytes
                                 #'source-region))
             (unwind-protect
                  (macrolet ((nest (&body b)
                               `(progn ,@b)))
                    ,@(when end
                        `((when (end ,source)
                            (assert (<= ,end (end ,source))))
                          (setf (end ,source) ,end)))
                    ,@body)
               (setf (pos ,source) ,p)
               ,@(when end
                   `((setf (end ,source) ,e1)))))))))

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
