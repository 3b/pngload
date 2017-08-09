(in-package :mediabox-png)

(defun get-stream (parse-data)
  (fast-io::input-buffer-stream (buffer parse-data)))

(defun get-data-path (parse-data)
  (let ((stream (get-stream parse-data)))
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
