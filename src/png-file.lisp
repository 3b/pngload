(in-package :mediabox-png)

(defclass png-file ()
  ((path :reader path
         :initarg :path)
   (buffer :reader buffer
           :initarg :buffer)
   (size :reader size
         :initarg :size)
   (datastream :reader datastream)))

(defmacro with-png-buffer ((buffer path) &body body)
  `(with-open-file (stream ,path :element-type 'octet)
     (with-fast-input (,buffer nil stream)
       ,@body)))

(defun get-file-size (buffer)
  (file-length (fast-io::input-buffer-stream buffer)))

(defun read-png-file (path)
  (with-png-buffer (buffer path)
    (let ((file (make-instance 'png-file
                               :path path
                               :buffer buffer
                               :size (get-file-size buffer))))
      (parse file :datastream)
      file)))
