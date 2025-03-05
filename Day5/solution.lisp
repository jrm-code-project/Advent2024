;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY5")

(defun read-input (input-file)
  (let ((lines (scan-file input-file #'read-line)))
    (let ((is-rule (#m(lambda (line) (find #\| line)) lines))
          (is-update (#m(lambda (line) (find #\, line)) lines)))
      (values (collect 'list (#m(lambda (rule)
                                  (map 'list #'parse-integer (str:split #\| rule)))
                                (choose is-rule lines)))
              (collect 'list (#m(lambda (update)
                                  (map 'list #'parse-integer (str:split #\, update)))
                                (choose is-update lines)))))))

(defun test-rule (rule update)
  (let ((left-position (position (first rule) update :test #'=))
        (right-position (position (second rule) update :test #'=)))
    (or (null left-position)
        (null right-position)
        (< left-position right-position))))

(defun test-rules (rules update)
  (collect-and
   (#mtest-rule
    (scan 'list rules)
    (series update))))

(defun middle-number (list)
  (elt list (/ (1- (length list)) 2)))

(defun part-1 ()
  (multiple-value-bind (rules updates) (read-input (input-pathname))
    (collect-sum
     (#mmiddle-number
      (choose-if
       (lambda (update) (test-rules rules update))
       (scan 'lists updates))))))

(defun sort-using-rules (rules list)
  (sort list (lambda (left right)
               (find (list left right) rules :test #'equal))))

(defun part-2 ()
  (multiple-value-bind (rules updates) (read-input (input-pathname))
    (collect-sum
     (#mmiddle-number
      (#m(lambda (update) (sort-using-rules rules update))
       (choose-if
        (lambda (update) (not (test-rules rules update)))
        (scan 'list updates)))))))

(defconstant +solution-1+ 4905)
(defconstant +solution-2+ 6204)
