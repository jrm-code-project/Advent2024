;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY10")

(defun read-grid (input-pathname)
  (read-file-into-grid #'char->decimal input-pathname))

(defun find-trailheads (grid)
  (gethash 0 (invert-grid grid)))

(defun take-step (grid location)
  (let ((target-elevation (1+ (grid-ref grid location))))
    (collect 'list
      (choose-if (lambda (loc)
                   (and (on-grid? grid loc)
                        (= (grid-ref grid loc) target-elevation)))
                 (scan 'list (list
                              (coord-north location)
                              (coord-east  location)
                              (coord-south location)
                              (coord-west  location)))))))

(defun trail-walker (grid)
  (lambda (trailhead)
    (collect-last
     (scan-fn 'list
              (lambda () (list trailhead))
              (lambda (frontiers)
                (remove-duplicates
                 (collect-append
                  (map-fn 'list #'take-step (series grid) (scan frontiers)))
                 :test #'equal))
              #'null))))

(defun scorer (collector)
  (lambda (grid)
    (let ((collect-trails (funcall collector grid)))
      (lambda (trailhead)
        (length (funcall collect-trails trailhead))))))

(defun puzzle (grid trailhead-scorer)
  (collect-sum
    (map-fn 'integer
            (funcall trailhead-scorer grid)
            (scan 'list (find-trailheads grid)))))

(defun part-1 ()
  (puzzle (read-grid (input-pathname)) (scorer #'trail-walker)))

(defun extend-path (grid path)
  (map 'list (lambda (step) (cons step path)) (take-step grid (car path))))

(defun trail-collector (grid)
  (lambda (trailhead)
    (collect-last
     (scan-fn 'list
              (lambda () (list (list trailhead)))
              (lambda (paths)
                (collect-append
                 (map-fn 'list
                         #'extend-path
                         (series grid)
                         (scan 'list paths))))
              (lambda (paths)
                (every (lambda (path)
                         (= (length path) 11))
                       paths))))))

(defun part-2 ()
  (puzzle (read-grid (input-pathname)) (scorer #'trail-collector)))

(defconstant +solution-1+ 841)
(defconstant +solution-2+ 1875)
