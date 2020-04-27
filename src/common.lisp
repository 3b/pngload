(in-package #:pngload)

(deftype ub8 () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub31 () '(unsigned-byte 31))
(deftype ub32 () '(unsigned-byte 32))
(deftype ub8a () '(simple-array ub8))
(deftype ub8a1d () '(simple-array ub8 (*)))
(deftype ub8a2d () '(simple-array ub8 (* *)))
(deftype ub8a3d () '(simple-array ub8 (* * *)))
(deftype ub16a () '(simple-array ub16))
(deftype ub16a1d () '(simple-array ub16 (*)))
(deftype ub16a2d () '(simple-array ub16 (* *)))
(deftype ub16a3d () '(simple-array ub16 (* * *)))
(deftype sb32 () '(signed-byte 32))

(defvar *png*)

(defstruct state
  decode-data
  flatten
  flip-y
  use-static-vector
  source
  mmap-pointer)

(defstruct (png (:conc-name nil))
  state
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

(defun get-path ()
  (source-path (state-source (state *png*))))
