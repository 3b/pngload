(in-package #:pngload)

(defstruct chunk
  length
  type
  data
  crc)

(defmethod print-object ((object chunk) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (get-chunk-type object))))

(defun get-chunk-type (chunk)
  (loop :with type = (chunk-type chunk)
        :for i :below 32 :by 8
        :collect (ldb (byte 8 i) type) :into result
        :finally (let* ((bytes (nreverse result))
                        (str (map 'string #'code-char bytes)))
                   (return
                     (values (if (every #'graphic-char-p str)
                                 str
                                 (coerce str 'list))
                             bytes)))))

(defun parse-chunk-data (png chunk)
  (case (chunk-type chunk)
    (#x49444154 (parse-chunk/idat png chunk))
    (#x49484452 (parse-chunk/ihdr png chunk))
    (#x49454e44 (parse-chunk/iend png chunk))
    (#x504c5445 (parse-chunk/plte png chunk))
    (#x6348524d (parse-chunk/chrm png chunk))
    (#x67414d41 (parse-chunk/gama png chunk))
    (#x69434350 (parse-chunk/iccp png chunk))
    (#x73424954 (parse-chunk/sbit png chunk))
    (#x73524742 (parse-chunk/srgb png chunk))
    (#x624b4744 (parse-chunk/bkgd png chunk))
    (#x68495354 (parse-chunk/hist png chunk))
    (#x74524e53 (parse-chunk/trns png chunk))
    (#x70485973 (parse-chunk/phys png chunk))
    (#x73504c54 (parse-chunk/splt png chunk))
    (#x74494d45 (parse-chunk/time png chunk))
    (#x69545874 (parse-chunk/itxt png chunk))
    (#x74455874 (parse-chunk/text png chunk))
    (#x7a545874 (parse-chunk/ztxt png chunk))
    (#x6f464673 (parse-chunk/offs png chunk))
    (#x7043414c (parse-chunk/pcal png chunk))
    (#x7343414c (parse-chunk/scal png chunk))
    (#x67494667 (parse-chunk/gifg png chunk))
    (#x67494678 (parse-chunk/gifx png chunk))
    (#x73544552 (parse-chunk/ster png chunk))
    (#x65584966 (parse-chunk/exif png chunk))
    (otherwise (parse-chunk/unknown png chunk))))

(defun parse-chunk (png)
  (let ((chunk (make-chunk)))
    (with-source ((state-source (state png)) :buffer nil)
      (setf (chunk-length chunk) (ub32be)
            (chunk-type chunk) (ub32be)
            (chunk-data chunk) (nest (parse-chunk-data png chunk))
            (chunk-crc chunk) (ub32be)))
    chunk))
