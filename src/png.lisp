(in-package :pngload)

(defclass png-object ()
  ((parse-tree :accessor parse-tree)
   (width :accessor width)
   (height :accessor height)
   (bit-depth :accessor bit-depth)
   (color-type :reader color-type)
   (palette-count :accessor palette-count
                  :initform 0)
   (palette :accessor palette :initform nil)
   (gamma :reader gamma
          :initform 1.0)
   (transparency :accessor transparency
                 :initform nil)
   (rendering-intent :reader rendering-intent
                     :initform nil)
   (compression-method :reader compression-method)
   (interlace-method :reader interlace-method)
   (filter-method :reader filter-method)
   (pixel-size :reader pixel-size
               :initform (list :x 1 :y 1 :unit :unknown))
   (last-modified :reader last-modified
                  :initform nil)
   (text :reader text
         :initform nil)
   (data :accessor data
         :initform nil)))

(defun load-stream (stream &key (decodep t) flattenp)
  (with-fast-input (*buffer* nil stream)
    (let ((*png-object* (make-instance 'png-object))
          (*decode-data* decodep)
          (*flattenp* flattenp))
      (setf (parse-tree *png-object*) (parse-datastream))
      *png-object*)))

(defun load-file (path &key (decodep t) flattenp)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in :decodep decodep :flattenp flattenp)))
