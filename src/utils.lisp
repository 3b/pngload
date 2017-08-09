(in-package :mediabox-png)

(defun get-stream (png-data)
  (fast-io::input-buffer-stream (buffer png-data)))

(defun get-data-path (png-data)
  (let ((stream (get-stream png-data)))
    (typecase stream
      (file-stream (pathname stream))
      (t :IN-MEMORY))))

(defun seek (png-data &optional offset (start :current))
  (with-slots (stream) (buffer png-data)
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

(defun buffer-data (png-data &key (bytes 1))
  (let ((octet-vector (make-octet-vector bytes)))
    (fast-read-sequence octet-vector (buffer png-data) 0 bytes)
    octet-vector))
