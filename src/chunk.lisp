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

(defun parse-chunk-data (chunk)
  (case (chunk-type chunk)
    (#x49444154 (parse-chunk/idat chunk))
    (#x49484452 (parse-chunk/ihdr chunk))
    (#x49454e44 (parse-chunk/iend chunk))
    (#x504c5445 (parse-chunk/plte chunk))
    (#x6348524d (parse-chunk/chrm chunk))
    (#x67414d41 (parse-chunk/gama chunk))
    (#x69434350 (parse-chunk/iccp chunk))
    (#x73424954 (parse-chunk/sbit chunk))
    (#x73524742 (parse-chunk/srgb chunk))
    (#x624b4744 (parse-chunk/bkgd chunk))
    (#x68495354 (parse-chunk/hist chunk))
    (#x74524e53 (parse-chunk/trns chunk))
    (#x70485973 (parse-chunk/phys chunk))
    (#x73504c54 (parse-chunk/splt chunk))
    (#x74494d45 (parse-chunk/time chunk))
    (#x69545874 (parse-chunk/itxt chunk))
    (#x74455874 (parse-chunk/text chunk))
    (#x7a545874 (parse-chunk/ztxt chunk))
    (#x6f464673 (parse-chunk/offs chunk))
    (#x7043414c (parse-chunk/pcal chunk))
    (#x7343414c (parse-chunk/scal chunk))
    (#x67494667 (parse-chunk/gifg chunk))
    (#x67494678 (parse-chunk/gifx chunk))
    (#x73544552 (parse-chunk/ster chunk))
    (#x65584966 (parse-chunk/exif chunk))
    (otherwise (parse-chunk/unknown chunk))))

(defun parse-chunk ()
  (let ((chunk (make-chunk)))
    (with-source ((state-source (state *png*)) :buffer nil)
      (setf (chunk-length chunk) (ub32be)
            (chunk-type chunk) (ub32be)
            (chunk-data chunk) (nest (parse-chunk-data chunk))
            (chunk-crc chunk) (ub32be)))
    chunk))
