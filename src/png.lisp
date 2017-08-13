(in-package :mediabox-png)

(defvar *png-object* nil)

(defclass png-object ()
  ((parse-tree :accessor parse-tree)
   (width :accessor width)
   (height :accessor height)
   (bit-depth :accessor bit-depth)
   (color-type :accessor color-type)
   (palette-count :accessor palette-count
                  :initform 0)
   (interlace-method :accessor interlace-method)
   (data :accessor data
         :initform nil)))

(defun read-png-stream (stream)
  (with-fast-input (*buffer* nil stream)
    (let ((*png-object* (make-instance 'png-object)))
      (setf (parse-tree *png-object*) (parse-datastream))
      *png-object*)))

(defun read-png-file (path)
  (with-open-file (in path :element-type 'ub8)
    (read-png-stream in)))
