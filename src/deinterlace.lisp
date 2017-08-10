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

(defun interlace-method-name (interlace-method)
  (case interlace-method
    (0 :null)
    (1 :adam7)))

(defun make-sub-images (parse-data)
  (loop :with width = (png-image-width (data parse-data))
        :with height = (png-image-height (data parse-data))
        :with sub-images = (make-array 7 :initial-element nil)
        :for x :from (1- width) :downto 0
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (list x y)
                            (aref sub-images
                                  (1- (aref +adam7-pattern+
                                            (mod y 8)
                                            (mod x 8))))))
        :finally (return sub-images)))

(defun deinterlace (parse-data)
  (make-sub-images parse-data))
