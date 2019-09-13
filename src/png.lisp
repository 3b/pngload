(in-package #:pngload)

(defstruct (png (:conc-name nil))
  parse-tree
  width
  height
  bit-depth
  color-type
  (palette-count 0)
  palette
  (gamma 1.0)
  transparency
  rendering-intent
  compression-method
  interlace-method
  filter-method
  (pixel-size '(:x 1 :y 1 :unit :unknown))
  last-modified
  text
  data)

(defun load-stream (stream &key (decode t) flatten flip-y static-vector)
  "load the given PNG datastream from STREAM. The following options are
  supported:

DECODE: When NIL, skip image decoding and only gather metadata.

FLATTEN: When non-NIL, read the image data into a 1-dimensional array.

FLIP-Y: When non-NIL, flip the image data on its Y axis while reading.

STATIC-VECTOR: When non-NIL, read the image data into a static-vectors array,
suitable to be passed to a foreign library.

See LOAD-FILE if you want to load a PNG datastream from a file on disk."
  (let ((*png-source* (make-instance 'stream-source :data stream)))
    (let ((*png* (make-png))
          (*decode-data* decode)
          (*flatten* flatten)
          (*flip-y* flip-y)
          (*use-static-vector* static-vector))
      (setf (parse-tree *png*) (parse-datastream))
      *png*)))

(defun load-file (path &key (decode t) flatten flip-y static-vector)
  "Load the PNG file located at the given filesystem PATH. The following options
  are supported:

DECODE: When NIL, skip image decoding and only gather metadata.

FLATTEN: When non-NIL, read the image data into a 1-dimensional array.

FLIP-Y: When non-NIL, flip the image data on its Y axis while reading.

STATIC-VECTOR: When non-NIL, read the image data into a static-vectors array,
suitable to be passed to a foreign library.

See LOAD-STREAM if you want to load a PNG datastream.
"
  (mmap:with-mmap (pointer fd size path)
    (3bz:with-octet-pointer (*mmap-pointer* pointer size)
      (let ((*png-source* (make-instance 'octet-pointer-source
                                         :data pointer
                                         :end size)))
        (let ((*png* (make-png))
              (*decode-data* decode)
              (*flatten* flatten)
              (*flip-y* flip-y)
              (*use-static-vector* static-vector))
          (setf (parse-tree *png*) (parse-datastream))
          *png*)))))

(defmacro with-png-in-static-vector ((png-var path-or-stream
                                      &key (decode t) flip-y) &body body)
  "Load a PNG image to a foreign array using static-vectors, automatically
freeing memory when finished.

See LOAD-STREAM
See LOAD-FILE"
  (alexandria:once-only (path-or-stream)
    `(let ((,png-var (if (streamp ,path-or-stream)
                         (load-stream ,path-or-stream
                                      :decode ,decode
                                      :flip-y ,flip-y
                                      :static-vector t
                                      :flatten t)
                         (load-file ,path-or-stream
                                    :decode ,decode
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
                  (loop :for i :below n
                        :collect (floor (+ (/ (* i 255) (1- n)) 1/2))))))

(defun expand-gray (png)
  (with-slots (width height bit-depth transparency color-type) png
    (when (and (member color-type '(:greyscale :greyscale-alpha))
               (< bit-depth 8))
      (let* ((*png* png)
             (channels (get-image-channels))
             (grey-channels (- channels (if transparency 1 0)))
             (data (data png))
             (stride (* width channels)))
        (flet ((data (y x c)
                 (row-major-aref data (+ (* y stride) (* x channels) c)))
               ((setf data) (v y x c)
                 (setf (row-major-aref data (+ (* y stride) (* x channels) c))
                       v)))
          (let ((map (make-grey-map bit-depth)))
            (dotimes (y height)
              (dotimes (x width)
                (dotimes (c grey-channels)
                  (let ((g (data y c x)))
                    (setf (data y x c) (aref map g)))))))))))
  png)
