(in-package #:pngload)

(defun load-file (path &key (decode t) flatten flip-y static-vector
                         unknown-chunk-warnings)
  "Load the PNG file located at the given filesystem PATH. The following options
  are supported:

DECODE: When NIL, skip image decoding and only gather metadata.

FLATTEN: When non-NIL, read the image data into a 1-dimensional array.

FLIP-Y: When non-NIL, flip the image data on its Y axis while reading.

STATIC-VECTOR: When non-NIL, read the image data into a static-vectors array,
suitable to be passed to a foreign library.

See LOAD-STREAM if you want to load a PNG datastream.
"
  (with-png-file (png path)
    (with-open-file (stream (path png) :element-type '3bz::octet)
      (let ((source (make-instance 'file-stream-source
                                   :data stream
                                   :end (file-length stream))))
        (setf (state-source (state png)) source
              (parse-tree png) (parse-datastream png))))
    png))
