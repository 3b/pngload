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

(defun calculate-sub-image-dimensions (image-width image-height)
  (loop :for pass :below 7
        :collect
        (flet ((calc (dim array)
                 (multiple-value-bind (w e) (floor dim 8)
                   (+ (* (aref (aref array pass) 8) w)
                      (aref (aref array pass) e)))))
          (list (calc image-width +adam7-widths+)
                (calc image-height +adam7-heights+)))))

(defun add-sub-image (dest pass w h image)
  (loop :with x1 = (1- (position 1 (aref +adam7-widths+ pass)))
        :with y1 = (1- (position 1 (aref +adam7-heights+ pass)))
        :with dx = (/ 8 (aref (aref +adam7-widths+ pass) 8))
        :with dy = (/ 8 (aref (aref +adam7-heights+ pass) 8))
        :for sy :below h
        :for y :from y1 :by dy
        :do (loop :for sx :below w
                  :for x :from x1 :by dx
                  :do (setf (aref dest y x)
                            (aref image sy sx)))))

(defun deinterlace ())
