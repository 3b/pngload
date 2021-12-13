(in-package #:pngload)

(defun load-vector (vector &key (decode t) flatten flip-y static-vector
                             unknown-chunk-warnings)
  "Load the PNG file from the given octet-vector. The following options
  are supported:

DECODE: When NIL, skip image decoding and only gather metadata.

FLATTEN: When non-NIL, read the image data into a 1-dimensional array.

FLIP-Y: When non-NIL, flip the image data on its Y axis while reading.

STATIC-VECTOR: When non-NIL, read the image data into a static-vectors array,
suitable to be passed to a foreign library.

See LOAD-STREAM if you want to load a PNG datastream.
"
  (let* ((state (make-state :decode-data decode
                            :flatten flatten
                            :flip-y flip-y
                            :use-static-vector static-vector
                            :unknown-chunk-warnings unknown-chunk-warnings))
         (png (make-png :state state))
         (source (make-instance 'octet-vector-source
                                :data vector
                                :end (array-dimension vector 0))))
    (setf (state-source (state png)) source
          (parse-tree png) (parse-datastream png))
    png))
