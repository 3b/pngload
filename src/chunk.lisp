(in-package :mediabox-png)

(defclass chunk ()
  ((length :reader chunk-length)
   (type :reader chunk-type)
   (data :reader chunk-data)
   (crc :reader chunk-crc)))

(defmethod print-object ((object chunk) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (chunk-type-name (chunk-type object)))))

(defun chunk-type-name (chunk-type)
  (case chunk-type
    (#x49484452 :ihdr)
    (#x504c5445 :plte)
    (#x49444154 :idat)
    (#x49454e44 :iend)
    (#x6348524d :chrm)
    (#x67414d41 :gama)
    (#x69434350 :iccp)
    (#x73424954 :sbit)
    (#x73524742 :srgb)
    (#x624b4744 :bkgd)
    (#x68495354 :hist)
    (#x74524e53 :trns)
    (#x70485973 :phys)
    (#x73504c54 :splt)
    (#x74494d45 :time)
    (#x69545874 :itxt)
    (#x74455874 :text)
    (#x7a545874 :ztxt)
    (#x65584966 :exif)
    (otherwise :unknown-chunk)))

(defun colour-type-name (colour-type)
  (ecase colour-type
    (0 :greyscale)
    (2 :truecolour)
    (3 :indexed-colour)
    (4 :greyscale-alpha)
    (6 :truecolour-alpha)))

(defmethod parse (parse-data (node (eql :chunk)) &key)
  (let ((@ (buffer parse-data))
        (chunk (make-instance 'chunk)))
    (with-slots (length type data crc) chunk
      (setf length (read-integer @ :bytes 4)
            type (read-integer @ :bytes 4)
            data (parse parse-data (chunk-type-name type) :length length)
            crc (read-integer @ :bytes 4)))
    chunk))
