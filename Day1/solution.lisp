;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY1")

(defun read-input (input-pathname)
  (multiple-value-bind (left-column right-column)
      (chunk 2 2 (scan-file input-pathname))
    (values (collect 'list left-column)
            (collect 'list right-column))))

(defun part-1 ()
  (multiple-value-bind (left-column right-column)
      (read-input (input-pathname))
    (collect-sum
     (#Mabs
      (#M-
       (scan (sort left-column #'<))
       (scan (sort right-column #'<)))))))

(defun part-2 ()
  (multiple-value-bind (left-column right-column)
      (read-input (input-pathname))
    (collect-sum
     (#M(lambda (item) (* item (count item right-column)))
        (scan left-column)))))

(defconstant +solution-1+ 1834060)
(defconstant +solution-2+ 21607792)
