;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY12")

(defun read-input (pathname)
  (read-file-into-grid
   (char-interner #'identity (find-package "ADVENT2024/DAY12"))
   pathname))

(defun neighbors (coord)
   (list (coord-north coord)
         (coord-south coord)
         (coord-east coord)
         (coord-west coord)))

(defun flood-fill (grid visited seed-point)
  (let ((region-tag (grid-ref grid seed-point)))
    (flet ((in-region? (coord)
             (and (on-grid? grid coord)
                  (eql region-tag (grid-ref grid coord)))))
      (let itr ((points (list seed-point))
                (area 0)
                (perimeter 0)
                (corners 0))
        (if (null points)
            (progn
              ;; (format t "~&id: ~S~%" region-tag)
              ;; (format t "~& area: ~S~%" area)
              ;; (format t "~& perimeter: ~S~%" perimeter)
              ;; (format t "~& segments: ~S~%" corners)
              (values area perimeter corners))
            (let ((this-point (car points))
                  (rest-points (cdr points)))
              (cond ((not (in-region? this-point))
                     (itr rest-points area (1+ perimeter) corners))
                    ((grid-ref visited this-point)
                     (itr rest-points area perimeter corners))
                    (t
                       (setf (grid-ref visited this-point) t)
                       (itr (append rest-points (neighbors this-point))
                            (1+ area)
                            perimeter
                            (+ corners
                               (let ((n*  (in-region? (coord-north     this-point)))
                                     (ne* (in-region? (coord-northeast this-point)))
                                     (e*  (in-region? (coord-east      this-point)))
                                     (se* (in-region? (coord-southeast this-point)))
                                     (s*  (in-region? (coord-south     this-point)))
                                     (sw* (in-region? (coord-southwest this-point)))
                                     (w*  (in-region? (coord-west      this-point)))
                                     (nw* (in-region? (coord-northwest this-point))))
                                 (+ (if (and n* e* (not ne*)) 1 0)
                                    (if (and e* s* (not se*)) 1 0)
                                    (if (and s* w* (not sw*)) 1 0)
                                    (if (and w* n* (not nw*)) 1 0)
                                    (if (and (not n*) (not e*)) 1 0)
                                    (if (and (not e*) (not s*)) 1 0)
                                    (if (and (not s*) (not w*)) 1 0)
                                    (if (and (not w*) (not n*)) 1 0)))))))))))))

(defun scan-unvisited (visited)
  (declare (optimizable-series-function))
  (choose
   (mapping (((coord val) (scan-grid visited)))
     (and (null val) coord))))

(defun scan-regions (grid)
  (declare (optimizable-series-function 3))
  (let ((visited (make-array (array-dimensions grid) :initial-element nil)))
    (#3M(lambda (seed) (flood-fill grid visited seed))
        (scan-unvisited visited))))

(defun puzzle (grid score-function)  
  (collect-sum
   (mapping (((area perimeter segments) (scan-regions grid)))
     (funcall score-function area perimeter segments))))

(defun part-1 ()
  (puzzle (read-input (input-pathname))
          (lambda (area perimeter segments)
            (declare (ignore segments))
            (* area perimeter))))

(defun part-2 ()
  (puzzle (read-input (input-pathname))
          (lambda (area perimeter segments)
            (declare (ignore perimeter))
            (* area segments))))

(defconstant +solution-1+ 1387004)
(defconstant +solution-2+ 844198)
