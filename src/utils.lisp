(in-package :mediabox-png)

(defun seek (file &optional offset (start :current))
  (with-slots (stream) (buffer file)
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

(defun read-octets (file &key (count 1))
  (let ((octets (make-octet-vector count)))
    (fast-read-sequence octets (buffer file) 0 count)
    octets))
