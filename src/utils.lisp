(in-package :mediabox-png)

(defun get-stream (buffer)
  (fast-io::input-buffer-stream buffer))

(defun get-vector (buffer)
  (fast-io::input-buffer-vector buffer))

(defun get-data-path (parse-data)
  (let ((stream (get-stream (buffer parse-data))))
    (typecase stream
      (file-stream (pathname stream))
      (t :IN-MEMORY))))

(defun seek (buffer &optional offset (start :current))
  (with-slots (stream) buffer
    (when offset
      (ecase start
        (:start (file-position stream offset))
        (:end (file-position stream (- (file-length stream) offset)))
        (:current (file-position stream (+ (file-position stream) offset)))))
    (file-position stream)))

(defun octets= (stream octet-list)
  (equalp stream (octets-from octet-list)))

(defun read-string (octets &key (encoding :latin-1))
  (let ((end (or (position 0 octets :test #'=) (length octets))))
    (babel:octets-to-string octets :end end :encoding encoding)))

(defmacro read-bytes (count buffer)
  `(fast-io::read-unsigned-be ,count ,buffer))

(defun buffer-data (buffer &key (bytes 1))
  (let ((octet-vector (make-octet-vector bytes)))
    (fast-read-sequence octet-vector buffer 0 bytes)
    octet-vector))

(defun read-until-null (octets start end)
  (let ((index (loop :for i :from start :below end
                     :when (zerop (aref octets i))
                       :do (return (1+ i))
                     :finally (return (1+ i)))))
    (values (subseq octets start index) index)))

(defun read-integer (octets start &key (bytes 1))
  (let ((value 0))
    (loop :for i :from (* (- bytes 1) 8) :downto 0 :by 8
          :for byte = (aref octets (+ i start))
          :collect (setf (ldb (byte 8 i) value) byte))
    value))
