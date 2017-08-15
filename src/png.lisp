(in-package :pngload)

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

(defun load-stream (stream &key (decodep t))
  (with-fast-input (*buffer* nil stream)
    (let ((*png-object* (make-instance 'png-object))
          (*decode-data* decodep))
      (setf (parse-tree *png-object*) (parse-datastream))
      *png-object*)))

(defun load-file (path &key (decodep t))
  (with-open-file (in path :element-type 'ub8)
    (load-stream in :decodep decodep)))
