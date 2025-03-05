;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY2")

(defun read-levels (stream eof-error-p eof-value)
  (let ((line (read-line stream eof-error-p eof-value)))
    (if (eq line eof-value)
        eof-value
        (with-input-from-string (stream line)
          (collect 'list (scan-stream stream))))))

(defun read-input (input-pathname)
  (collect 'list (scan-file input-pathname #'read-levels)))

(defun deltas (series)
  (declare (optimizable-series-function)
           (off-line-port series))
  (multiple-value-bind (left right) (chunk 2 1 series)
    (#m- right left)))

(defun safe-levels? (list)
  (let ((deltas (deltas (scan list))))
    (let ((ascending (collect-and (#mplusp deltas)))
          (descending (collect-and (#mminusp deltas)))
          (small (collect-and (#m<= (#mabs deltas) (series 3)))))
      (and small
           (or ascending descending)))))

(defun part-1 ()
  (count-if #'safe-levels? (read-input (input-pathname))))

(defun safe-dampened-levels? (levels)
  (find-if #'safe-levels? (remove-one-element levels)))

(defun part-2 ()
  (count-if #'safe-dampened-levels? (read-input (input-pathname))))

(defconstant +solution-1+ 242)
(defconstant +solution-2+ 311)
