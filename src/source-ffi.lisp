(in-package :pngload)

(declaim (inline ub32->sb32))
(defun ub32->sb32 (s)
  (if (logbitp 31 s)
      (dpb s (byte 32 0) -1)
      s))

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
                  (prog1 (cffi:mem-ref ,v :uint8 ,p)
                    (incf ,p)))
                (ub16be ()
                  (assert (<= (+ ,p 2) ,e))
                  (prog1 (be16 (cffi:mem-ref ,v :uint16 ,p))
                    (incf ,p 2)))
                (ub32be ()
                  (assert (<= (+ ,p 4) ,e))
                  (prog1 (be32 (cffi:mem-ref ,v :uint32 ,p))
                    (incf ,p 4)))
                (sb32be ()
                  (assert (<= (+ ,p 4) ,e))
                  (prog1 (ub32->sb32 (be32 (cffi:mem-ref ,v :uint32 ,p)))
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
                  (prog1 (.source-region ,source n)
                    (setf ,p (+ ,p n))))
                (read-string (&key (bytes (- ,e ,p)) encoding
                                null-terminated-p zlib)
                  (let ((s (make-array bytes :element-type 'ub8)))
                    (loop :for i :below bytes
                          :for b = (ub8)
                          :do (setf (aref s i) b)
                          :until (and null-terminated-p (zerop b)))
                    (when null-terminated-p
                      (setf s (subseq s 0 (position 0 s))))
                    (when zlib
                      (setf s (3bz:decompress-vector s :format :zlib)))
                    (babel:octets-to-string s :encoding encoding))))
         (declare (inline ub8 ub16be ub32be sb32be)
                  (ignorable #'ub8 #'ub16be #'ub32be #'sb32be
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
