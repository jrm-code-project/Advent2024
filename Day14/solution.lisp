;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY14")

(defun read-input (input-file)
  (collect 'list
    (#M(lambda (line)
         (cl-ppcre:register-groups-bind ((#'parse-integer column) (#'parse-integer row)
                                         (#'parse-integer dx) (#'parse-integer dy))
             ("p=(\\d+),(\\d+)\\s+v=(-?\\d+),(-?\\d+)" line)
           (list (coord column row)
                 (coord dx dy))))
       (scan-file input-file #'read-line))))

(defun step-n (width height coord velocity n)
  (2v-mod (2v+ coord (2v* n velocity)) (coord width height)))

(defun quadrant (width height coord)
  (let ((half-width (floor width 2))
        (half-height (floor height 2)))
    (cond ((< (row coord) half-height)
           (cond ((< (column coord) half-width) 1)
                 ((> (column coord) half-width) 2)
                 (t nil)))
          ((> (row coord) half-height)
           (cond ((> (column coord) half-width) 3)
                 ((< (column coord) half-width) 4)
                 (t nil)))
          (t nil))))

(defparameter +grid-width+ 101)
(defparameter +grid-height+ 103)

(defun part-1 ()
  (let ((quadrants
          (#M(lambda (coord)
               (quadrant +grid-width+ +grid-height+ coord))
             (#M(lambda (robot)
                  (step-n +grid-width+ +grid-height+ (first robot) (second robot) 100))
                (scan 'list (read-input (input-pathname)))))))
    (* (collect-sum (#M(lambda (q) (if (eql q 1) 1 0)) quadrants))
       (collect-sum (#M(lambda (q) (if (eql q 2) 1 0)) quadrants))
       (collect-sum (#M(lambda (q) (if (eql q 3) 1 0)) quadrants))
       (collect-sum (#M(lambda (q) (if (eql q 4) 1 0)) quadrants)))))

(defun occupied-row? (locs row)
  (find row locs :test #'= :key #'row))

(defun occupied-column? (locs column)
  (find column locs :test #'= :key #'column))

(defun score (locs)
  (+ (collect-length (choose-if #'not (#Moccupied-row? (series locs) (scan-range :from 0 :below +grid-height+))))
     (collect-length (choose-if #'not (#Moccupied-column? (series locs) (scan-range :from 0 :below +grid-width+))))))

(defun part-2 ()
  (let ((robots (read-input (input-pathname))))
    (caar
     (sort
      (collect 'list
        (#M(lambda (n)
             (cons n (score (map 'list (lambda (robot)
                                         (step-n +grid-width+ +grid-height+ (first robot) (second robot) n))
                                 robots))))
           (scan-range :from 0 :below (* +grid-width+ +grid-height+))))
      #'> :key #'cdr))))

(defconstant +solution-1+ 218433348)
(defconstant +solution-2+ 6512)
