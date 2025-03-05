;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY4")

(defparameter *test-grid-1*
  #2A((\. \. X \. \. \.)
      (\.  S A  M  X \.)
      (\.  A \. \. A \.)
      (X   M  A  S \. S)
      (\.  X \. \. \. \.)))

(defparameter *test-grid-2*
              #2A((M M M S X X M A S M)
                  (M S A M X M S M S A)
                  (A M X S X M A A M M)
                  (M S A M A S M S M X)
                  (X M A S A M X A M M)
                  (X X A M M X X A M A)
                  (S M S M S A S X S S)
                  (S A X A M A S A A A)
                  (M A M M M X M M M M)
                  (M X M X A X M A S X)))

(defun read-input (input-pathname)
  (read-file-into-grid (char-interner #'char-upcase (find-package "ADVENT2024/DAY4")) input-pathname))

(defun search-trace (trace target)
  (let ((rev (reverse target)))
    (collect-sum
     (#m(lambda (suffix)
          (if (or (starts-with-subseq target suffix)
                  (starts-with-subseq rev suffix))
              1
              0))
        (scan-suffixes trace)))))

(defun search-trace-list (trace-list target)
  (collect-sum
   (#m(lambda (trace)
        (search-trace trace target))
    (scan 'list trace-list))))

(defun search-grid (grid target)
  (collect-sum
   (#M(lambda (grid trace)
        (search-trace-list (funcall trace grid) target))
      (series grid)
      (scan 'list (list (lambda (grid) (declare (ignorable grid)) (collect 'list (scan-rows grid)))
                        (lambda (grid) (declare (ignorable grid)) (collect 'list (scan-columns grid)))
                        (lambda (grid) (declare (ignorable grid)) (collect 'list (scan-falling-diagonals grid)))
                        (lambda (grid) (declare (ignorable grid)) (collect 'list (scan-rising-diagonals grid))))))))

(defun part-1 ()
  (search-grid (read-input (input-pathname)) #(X M A S)))

(defun m-s1? (grid coord1 coord2)
  (and (on-grid? grid coord1)
       (on-grid? grid coord2)
       (eql (grid-ref grid coord1) 'M)
       (eql (grid-ref grid coord2) 'S)))

(defun m-s? (grid coord1 coord2)
  (or (m-s1? grid coord1 coord2)
      (m-s1? grid coord2 coord1)))

(defun x-mas? (grid coord)
  (and (on-grid? grid coord)
       (eql (grid-ref grid coord) 'A)
       (and (m-s? grid (coord-northwest coord) (coord-southeast coord))
            (m-s? grid (coord-northeast coord) (coord-southwest coord)))))

(defun search-x-mas (grid)
  (collect-sum
   (#m(lambda (coord)
        (if (x-mas? grid coord)
            1
            0))
      (scan-grid-coords grid))))

(defun part-2 ()
  (search-x-mas (read-input (input-pathname))))

(defconstant +solution-1+ 2530)
(defconstant +solution-2+ 1921)
