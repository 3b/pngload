(in-package #:pngload)

(defun get-channel-count (png)
  (ecase (color-type png)
    (:truecolour 3)
    (:truecolour-alpha 4)
    (:indexed-colour 1)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun get-sample-bytes (png)
  (max 1 (/ (bit-depth png) 8)))

(defun get-pixel-bytes (png)
  (* (get-sample-bytes png) (get-channel-count png)))

(defun get-scanline-bytes (png width)
  (ceiling (* (bit-depth png)
              (get-channel-count png)
              width)
           8))
