(in-package :cl-user)

(defpackage #:pngload.test
  (:use #:cl
        #:pngload)
  (:export #:test-images
           #:test-read-times))

(in-package :pngload.test)

(defvar *failed* nil)

(defun get-path ()
  (uiop:ensure-directory-pathname
   (asdf:system-relative-pathname :pngload "test")))

(defun get-image-name (file)
  (namestring
   (make-pathname
    :defaults
    (pathname-name file)
    :type (pathname-type file))))

;;todo: combine normal/flip/flatten into 1 function to allow testing
;;combinations
(defun test-images ()
  (let ((*failed*)
        (files (uiop:directory-files (get-path))))
    (flet ((test-image (file)
             (let ((image (ignore-errors (load-file file)))
                   (opticl (opticl:read-image-file file)))
               (unless (and image
                            (equalp (data image) opticl))
                 (push (get-image-name file) *failed*)))))
      (map nil #'test-image files)
      (format t "Passed (~D)" (- (length files) (length *failed*)))
      (format t "~&Failed (~D): ~A" (length *failed*) *failed*))))

(defun test-images-flip ()
  (let ((*failed*)
        (files (uiop:directory-files (get-path))))
    (flet ((test-image (file)
             (let ((image (ignore-errors (load-file file :flip-y t)))
                   (opticl (opticl:vertical-flip-image
                            (opticl:read-image-file file))))
               (unless (and image
                            (equalp (data image) opticl))
                 (push (get-image-name file) *failed*)))))
      (map nil #'test-image files)
      (format t "Passed (~D)" (- (length files) (length *failed*)))
      (format t "~&Failed (~D): ~A" (length *failed*) *failed*))))

(defun test-images-flatten ()
  (let ((*failed*)
        (files (uiop:directory-files (get-path))))
    (flet ((test-image (file)
             (let* ((image (progn (load-file file :flatten t)))
                    (opticl* (opticl:read-image-file file))
                    (opticl (make-array (array-total-size opticl*)
                                        :element-type (array-element-type opticl*)
                                        :displaced-to opticl*)))
               (unless (and image
                            (equalp (data image) opticl))
                 (format t "~&opticl:~% ~s~%" opticl)
                 (format t "~&image:~% ~s~%" image)
                 (push (get-image-name file) *failed*)))))
      (map nil #'test-image files)
      (format t "Passed (~D)" (- (length files) (length *failed*)))
      (format t "~&Failed (~D): ~A" (length *failed*) *failed*))))

(defun test-read-time (library-name func file count)
  (let ((start (local-time:now)))
    (dotimes (i count)
      (funcall func file))
    (format t "~A: ~,3fs~%"
            library-name
            (local-time:timestamp-difference
             (local-time:now)
             start))))

(defun test-read-times (file &key (count 1))
  (test-read-time "pngload" #'load-file file count)
  (test-read-time "opticl" #'opticl:read-image-file file count)
  (test-read-time "cl-png" #'png::decode-file file count))
