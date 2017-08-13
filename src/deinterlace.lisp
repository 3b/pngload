(in-package :mediabox-png)

(define-constant +adam7-pattern+
  #2a((1 6 4 6 2 6 4 6)
      (7 7 7 7 7 7 7 7)
      (5 6 5 6 5 6 5 6)
      (7 7 7 7 7 7 7 7)
      (3 6 4 6 3 6 4 6)
      (7 7 7 7 7 7 7 7)
      (5 6 5 6 5 6 5 6)
      (7 7 7 7 7 7 7 7))
  :test #'equalp)

(define-constant +adam7-widths+
  #(#(0 1 1 1 1 1 1 1 1)
    #(0 0 0 0 0 1 1 1 1)
    #(0 1 1 1 1 2 2 2 2)
    #(0 0 0 1 1 1 1 2 2)
    #(0 1 1 2 2 3 3 4 4)
    #(0 0 1 1 2 2 3 3 4)
    #(0 1 2 3 4 5 6 7 8))
  :test #'equalp)

(define-constant +adam7-heights+
  #(#(0 1 1 1 1 1 1 1 1)
    #(0 1 1 1 1 1 1 1 1)
    #(0 0 0 0 0 1 1 1 1)
    #(0 1 1 1 1 2 2 2 2)
    #(0 0 0 1 1 1 1 2 2)
    #(0 1 1 2 2 3 3 4 4)
    #(0 0 1 1 2 2 3 3 4))
  :test #'equalp)

(defun interlace-method-name (interlace-method)
  (case interlace-method
    (0 :null)
    (1 :adam7)))

(defun get-sub-image-bytes (width height)
  (* height (ceiling (* width (/ (get-pixel-bytes) 8)) 8)))

(defun calculate-sub-image-dimensions (image-width image-height)
  (loop :for pass :below 7
        :collect
        (flet ((calc (dim array)
                 (multiple-value-bind (w e) (floor dim 8)
                   (+ (* (aref (aref array pass) 8) w)
                      (aref (aref array pass) e)))))
          (list (calc image-width +adam7-widths+)
                (calc image-height +adam7-heights+)))))

(defun add-sub-image/sub-byte (dest pass w h image pixel-bits)
  (error "not done yet"))

(defun add-sub-image (dest source pass w h pixel-bytes start)
  (loop :with x1 = (1- (position 1 (aref +adam7-widths+ pass)))
        :with y1 = (1- (position 1 (aref +adam7-heights+ pass)))
        :with dx = (/ 8 (aref (aref +adam7-widths+ pass) 8))
        :with dy = (/ 8 (aref (aref +adam7-heights+ pass) 8))
        :for sy :below h
        :for y :from y1 :by dy
        :for dyb = (* y (* (image-width *png-object*)) pixel-bytes)
        :for syb = (+ start (* sy w pixel-bytes))
        :do (loop :for sx :below (* pixel-bytes w) :by pixel-bytes
                  :for x :from (* x1 pixel-bytes) :by (* dx pixel-bytes)
                  :do (loop :for i :below pixel-bytes
                            do (setf (aref dest (+ dyb x i))
                                     (aref source (+ syb sx i)))))))

(defun deinterlace-adam7 (data)
  (loop :with pixel-bits = (* (bit-depth *png-object*) (get-channel-count))
        :with width = (image-width *png-object*)
        :with height = (image-height *png-object*)
        :with dest = (make-array (get-image-bytes)
                                 :element-type 'ub8
                                 :initial-element #xff)
        :for pass :below 7
        :for start = 0 :then skip
        :for (sw sh) :in (calculate-sub-image-dimensions width height)
        :for skip = (+ start sh (get-sub-image-bytes sw sh))
        :do (unfilter data sh start)
            (if (< pixel-bits 8)
                (add-sub-image/sub-byte dest data pass sw sh pixel-bits)
                (add-sub-image dest data pass sw sh (/ pixel-bits 8) start))
        :finally (return dest)))
