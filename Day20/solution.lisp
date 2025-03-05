;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY20")

(defun read-input (input-pathname)
  (read-file-into-grid (char-interner #'identity (find-package "ADVENT2024/DAY20")) input-pathname))

(defun find-start-and-goal (maze)
  (let ((inverse (invert-grid maze '|.|)))
    (values (car (gethash 'S inverse))
            (car (gethash 'E inverse)))))

(defun compute-distances (maze)
  (let ((distances (make-grid (grid-height maze) (grid-width maze)
                              :initial-element nil)))
    (multiple-value-bind (start goal) (find-start-and-goal maze)
      (declare (ignore start))
      (let iter ((current goal)
                 (distance 0))
        (when current
          (setf (grid-ref distances current) distance)
          (iter (let* ((neighbors (#M2v+ (scan 'list (list +north+ +south+ +east+ +west+))
                                         (series current)))
                       (fill? (#M(lambda (maze neighbor)
                                   (and (on-grid? maze neighbor)
                                        (not (eql (grid-ref maze neighbor) '\#))
                                        (null (grid-ref distances neighbor))))
                                 (series maze)
                                 neighbors)))
                  (collect-first (choose fill? neighbors)))
            (1+ distance))))
      distances)))

(defun scan-square-coords (size)
  (declare (optimizable-series-function))
  (let ((displacement (coord size size)))
    (#M2v- (scan-coords (1+ (* size 2)) (1+ (* size 2)))
           (series displacement))))

(defun count-location-cheats (distances coord distance cheat-steps threshold)
  (collect-sum
   (choose
    (mapping (((cheat-vec) (scan-square-coords cheat-steps)))
      (let ((manhattan-distance (+ (abs (column cheat-vec)) (abs (row cheat-vec))))
            (cheat-coord (2v+ coord cheat-vec)))
        (and (<= manhattan-distance cheat-steps)
             (on-grid? distances cheat-coord)
             (let ((cheat-distance (grid-ref distances cheat-coord)))
               (and cheat-distance
                    (let* ((distance-if-cheating (+ manhattan-distance cheat-distance))
                           (savings (- distance distance-if-cheating)))
                      (and (>= savings threshold)
                           1))))))))))  

(defun count-cheats (distances-grid cheat-steps threshold)
  (collect-sum
   (choose
    (mapping (((coord distance) (scan-grid distances-grid)))
      (and distance
           (count-location-cheats distances-grid coord distance cheat-steps threshold))))))

(defun part-1 ()
  (count-cheats (compute-distances (read-input (input-pathname))) 2 100))

(defun part-2 ()
  (count-cheats (compute-distances (read-input (input-pathname))) 20 100))

(defparameter +solution-1+ 1497)
(defparameter +solution-2+ 1030809)
