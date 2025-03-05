;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY3")

(defun read-input (input-pathname)
  (read-file-into-string input-pathname))

(defparameter *mul-instruction* "(mul\\((\\d{1,3}),(\\d{1,3})\\))")

(defun part-1 ()
  (let ((answer 0))
    (cl-ppcre:do-register-groups (whole (#'parse-integer left) (#'parse-integer right))
        (*mul-instruction* (read-input (input-pathname)))
      (declare (ignore whole))
      (incf answer (* left right)))
    answer))

(defparameter *do-mul-instruction* "(do\\(\\))|(don't\\(\\))|(mul\\((\\d{1,3}),(\\d{1,3})\\))")

(defun part-2 ()
  (let ((answer 0)
        (on t))
    (cl-ppcre:do-register-groups (turn-on turn-off whole (#'parse-integer left) (#'parse-integer right))
        (*do-mul-instruction* (read-input (input-pathname)))
      (declare (ignore whole))
      (cond (turn-on (setq on t))
            (turn-off (setq on nil))
            (on (incf answer (* left right)))
            (t nil)))
    answer))

(defconstant +solution-1+ 166357705)
(defconstant +solution-2+ 88811886)
