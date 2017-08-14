(in-package :cl-user)

(defpackage #:mediabox-png.test
  (:use #:cl
        #:mediabox-png)
  (:export #:test-images
           #:test-read-times))

(in-package :mediabox-png.test)

(defvar *failed* nil)

(defun get-path ()
  (uiop:ensure-directory-pathname
   (asdf:system-relative-pathname :mediabox-png "test")))

(defun get-image-name (file)
  (namestring
   (make-pathname
    :defaults
    (pathname-name file)
    :type (pathname-type file))))

(defun print-image-data (file)
  (let ((image (ignore-errors (read-png-file file))))
    (unless image
      (push (get-image-name file) *failed*))))

(defun test-images ()
  (let ((*failed*)
        (files (uiop:directory-files (get-path))))
    (map nil #'print-image-data files)
    (format t "Passed (~D)" (- (length files) (length *failed*)))
    (format t "~&Failed (~D): ~A" (length *failed*) *failed*)))

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
  (test-read-time "mediabox-png" #'read-png-file file count)
  (test-read-time "png-read" #'png-read:read-png-file file count)
  (test-read-time "cl-png" #'png::decode-file file count)
  (test-read-time "sdl2-image" (lambda (x)
                                 (sdl2:free-surface
                                  (sdl2-image:load-image x)))
                  file count)
  (test-read-time "cl-soil" (lambda (x)
                              (cl-soil:free-image-data
                               (cl-soil:load-image x)))
                  file count))
