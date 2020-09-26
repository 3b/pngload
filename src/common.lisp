(in-package #:pngload)

(deftype ub8 () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype ub8a () '(simple-array ub8))
(deftype ub8a1d () '(simple-array ub8 (*)))
(deftype ub8a2d () '(simple-array ub8 (* *)))
(deftype ub8a3d () '(simple-array ub8 (* * *)))
(deftype ub16a1d () '(simple-array ub16 (*)))
(deftype ub16a2d () '(simple-array ub16 (* *)))
(deftype ub16a3d () '(simple-array ub16 (* * *)))
(deftype sb32 () '(signed-byte 32))

(defstruct state
  decode-data
  flatten
  flip-y
  use-static-vector
  unknown-chunk-warnings
  source
  mmap-pointer
  interlace-method
  palette
  transparency)

(defstruct (png (:conc-name nil))
  path
  state
  parse-tree
  width
  height
  bit-depth
  color-type
  data)

(defmethod print-object ((object png) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~ax~a @ ~abpp: ~a"
            (width object)
            (height object)
            (* (bit-depth object)
               (get-channel-count object))
            (path object))))
