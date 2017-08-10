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

(defun deflate-octets (octets)
  (chipz:decompress nil :zlib octets))

(defun read-octet (buffer)
  (fast-read-byte buffer))

(defun read-bytes (buffer &key (count 1) deflatep)
  (let ((sequence (make-octet-vector count)))
    (fast-read-sequence sequence buffer)
    (if deflatep (deflate-octets sequence) sequence)))

(defun read-integer (buffer &key (bytes 1))
  (let ((value 0))
    (loop :for i :from (* (1- bytes) 8) :downto 0 :by 8
          :for byte = (read-octet buffer)
          :collect (setf (ldb (byte 8 i) value) byte))
    value))

(defun read-string (buffer &key bytes nullp deflatep (encoding :latin-1))
  (let* ((start (buffer-position buffer))
         (octets (get-vector buffer))
         (max-length (or bytes (length octets)))
         (end (min (length octets) (+ start max-length)))
         (index (if nullp (position 0 octets :start start :end end) end))
         (sequence (make-octet-vector (- index start))))
    (fast-read-sequence sequence buffer)
    (when nullp (read-octet buffer))
    (babel:octets-to-string (if deflatep (deflate-octets sequence) sequence)
                            :encoding encoding)))
