(in-package #:pngload)

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
    (3bz:with-octet-pointer (pointer-binding pointer size)
      (let* ((source (make-instance 'octet-pointer-source
                                    :data pointer
                                    :end size))
             (state (make-state :decode-data decode
                                :flatten flatten
                                :flip-y flip-y
                                :use-static-vector static-vector
                                :source source
                                :mmap-pointer pointer-binding))
             (png (make-png :state state)))
        (setf (parse-tree png) (parse-datastream png))
        png))))
