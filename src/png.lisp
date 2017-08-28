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

(defun load-stream (stream &key (decode t) flatten flip-y static-vector)
  (with-fast-input (*buffer* nil stream)
    (let ((*png-object* (make-instance 'png-object))
          (*decode-data* decode)
          (*flatten* flatten)
          (*flip-y* flip-y)
          (*use-static-vector* static-vector))
      (setf (parse-tree *png-object*) (parse-datastream))
      *png-object*)))

(defun load-file (path &key (decode t) flatten flip-y static-vector)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in :decode decode :flatten flatten :flip-y flip-y
                 :static-vector static-vector)))

(defmacro with-png-in-static-vector ((png-var path-or-stream
                                      &key (decode t) flip-y)
                                     &body body)
  (once-only (path-or-stream)
    `(let ((,png-var (if (streamp ,path-or-stream)
                         (load-stream ,path-or-stream :decode ,decode
                                                      :flip-y ,flip-y
                                                      :static-vector t
                                                      :flatten t)
                         (load-file ,path-or-stream :decode ,decode
                                                    :flip-y ,flip-y
                                                    :static-vector t
                                                    :flatten t))))
       (unwind-protect
            (progn ,@body)
         (when (and ,png-var (data ,png-var))
           (static-vectors:free-static-vector (data ,png-var))
           (setf (data ,png-var) nil))))))

(defun make-grey-map (depth)
  (assert (< depth 8))
  (let ((n (expt 2 depth)))
    (make-array n :element-type '(unsigned-byte 8)
                    :initial-contents
                    (loop for i below n
                          collect (floor (+ (/ (* i 255) (1- n)) 1/2))))))

(defun expand-gray (png)
  (with-slots (width height bit-depth transparency color-type) png
    (when (and (member color-type '(:greyscale :greyscale-alpha))
               (< bit-depth 8))
      (let* ((*png-object* png)
             (channels (get-image-channels))
             (grey-channels (- channels (if transparency 1 0)))
             (data (data png))
             (stride (* width channels)))
        (flet ((data (y x c)
                 (row-major-aref data (+ (* y stride) (* x channels) c)))
               ((setf data) (v y x c)
                 (setf (row-major-aref data (+ (* y stride) (* x channels) c))
                       v)))
          (loop with map = (make-grey-map bit-depth)
                for y below height
                do (loop for x below width
                         do (loop for c below grey-channels
                                  for g = (data y x c)
                                  do (setf (data y x c)
                                           (aref map g)))))))))
  png)
