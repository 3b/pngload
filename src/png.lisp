(in-package :mediabox-png)

(defclass png-data ()
  ((buffer :reader buffer
           :initarg :buffer)
   (datastream :reader datastream)))

(defun read-png-stream (stream)
  (with-fast-input (buffer nil stream)
    (let ((png-data (make-instance 'png-data :buffer buffer)))
      (parse png-data :datastream)
      png-data)))

(defun read-png-file (path)
  (with-open-file (in path :element-type 'octet)
    (read-png-stream in)))
