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

(defun buffer-data (buffer &key (bytes 1))
  (let ((octet-vector (make-octet-vector bytes)))
    (fast-read-sequence octet-vector buffer 0 bytes)
    octet-vector))

(defmacro read-integer (buffer &key (bytes 1))
  `(fast-io::read-unsigned-be ,bytes ,buffer))

(defun read-string (buffer &key bytes nullp deflatep (encoding :latin-1))
  (let* ((start (buffer-position buffer))
         (octets (get-vector buffer))
         (max-length (if nullp (1+ bytes) (or bytes (length octets))))
         (end (min (length octets) (+ start max-length)))
         (index (if nullp (position 0 octets :start start :end end) end))
         (sequence (make-octet-vector (- index start))))
    (fast-read-sequence sequence buffer)
    (when nullp
      (fast-read-byte buffer))
    (babel:octets-to-string (if deflatep (deflate-octets sequence) sequence)
                            :encoding encoding)))
