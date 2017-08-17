(in-package :pngload)

(deftype ub8 () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype ub8a () '(simple-array ub8))
(deftype ub8a1d () '(simple-array ub8 (*)))
(deftype ub8a2d () '(simple-array ub8 (* *)))
(deftype ub8a3d () '(simple-array ub8 (* * *)))
(deftype ub16a () '(simple-array ub16))
(deftype ub16a1d () '(simple-array ub16 (*)))
(deftype ub16a2d () '(simple-array ub16 (* *)))
(deftype ub16a3d () '(simple-array ub16 (* * *)))

(defvar *buffer*)
(defvar *decode-data*)
(defvar *png-object*)

(defun get-path ()
  (let ((stream (fast-io::input-buffer-stream *buffer*)))
    (typecase stream
      (file-stream (pathname stream))
      (t :IN-MEMORY))))

(defun octets= (octet-vector octet-list)
  (equalp octet-vector (octets-from octet-list)))

(defun deflate-octets (octet-vector)
  (chipz:decompress nil 'chipz:zlib octet-vector
                    :buffer-size (* 2 (length octet-vector))))

(defun read-bytes (count &key deflatep)
  (let ((sequence (make-octet-vector count)))
    (fast-read-sequence sequence *buffer*)
    (if deflatep (deflate-octets sequence) sequence)))

(defun read-integer (&key (bytes 1))
  (ecase bytes
    (1 (fast-read-byte *buffer*))
    (2 (fast-io::read-unsigned-be 2 *buffer*))
    (4 (fast-io::read-unsigned-be 4 *buffer*))))

(defun read-string (&key bytes nullp deflatep (encoding :latin-1))
  (let* ((start (buffer-position *buffer*))
         (octets (fast-io::input-buffer-vector *buffer*))
         (max-length (or bytes (length octets)))
         (end (min (length octets) (+ start max-length)))
         (index (if nullp (position 0 octets :start start :end end) end))
         (sequence (make-octet-vector (- index start))))
    (fast-read-sequence sequence *buffer*)
    (when nullp (read-integer :bytes 1))
    (babel:octets-to-string (if deflatep (deflate-octets sequence) sequence)
                            :encoding encoding)))
