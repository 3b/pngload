(in-package #:pngload)

(defun get-channel-count ()
  (ecase (color-type *png*)
    (:truecolour 3)
    (:truecolour-alpha 4)
    (:indexed-colour 1)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun get-sample-bytes ()
  (max 1 (/ (bit-depth *png*) 8)))

(defun get-pixel-bytes ()
  (* (get-sample-bytes) (get-channel-count)))

(defun get-scanline-bytes (width)
  (ceiling (* (bit-depth *png*)
              (get-channel-count)
              width)
           8))
