(in-package #:pngload)

(defstruct chunk
  length
  type
  data
  crc)

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
    (otherwise (parse-chunk/unknown chunk))))

(defun parse-chunk ()
  (let ((chunk (make-chunk)))
    (with-source (*png-source* :buffer nil)
      (with-slots (length type data crc) chunk
        (setf length (ub32be)
              type (ub32be)
              data (nest (parse-chunk-data chunk))
              crc (ub32be))))
    chunk))
