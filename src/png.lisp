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
   (palette :accessor palette :initform nil)
   (transparency :accessor transparency
                 :initform nil)
   (interlace-method :accessor interlace-method)
   (data :accessor data
         :initform nil)))

(defun load-stream (stream)
  (with-fast-input (*buffer* nil stream)
    (let ((*png-object* (make-instance 'png-object)))
      (setf (parse-tree *png-object*) (parse-datastream))
      *png-object*)))

(defun load-file (path)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in)))
