;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY6")

(defun read-input (pathname)
  (read-file-into-grid (char-interner #'char-upcase (find-package "ADVENT2024/DAY6")) pathname))

(defun get-initial-position (grid)
  (let ((coord (collect-first
                (choose-if
                 (lambda (coord) (member (grid-ref grid coord) '(^ < > v)))
                 (scan-grid-coords grid)))))
    (ocoord coord
           (ecase (grid-ref grid coord)
             (^ +north+)
             (> +east+)
             (v +south+)
             (< +west+)))))

(defun patrol-step (grid obstacle-coord oriented-position)
  (let ((next-ocoord (ocoord-advance oriented-position)))
    (cond ((not (on-grid? grid (ocoord-coord next-ocoord))) nil)
          ((or (eq (grid-ref grid (ocoord-coord next-ocoord)) '|#|)
               (equal (ocoord-coord next-ocoord) obstacle-coord))
           (ocoord-cw oriented-position))
          (t next-ocoord))))

(defun patrol (grid obstacle-coord start-opos)
  (let ((history-hash (make-hash-table :test 'equal)))
    (setf (gethash start-opos history-hash) t)
    (let iter ((opos start-opos)
               (history (list start-opos)))
      (let ((next (patrol-step grid obstacle-coord opos)))
        (cond ((null next) history)
              ((gethash next history-hash nil) :loop)
              (t (setf (gethash next history-hash) t)
                 (iter next (cons next history))))))))

(defun unique-cells (history)
  (length (remove-duplicates (map 'list #'ocoord-coord history) :test #'equal)))

(defun part-1 ()
  (let* ((grid (read-input (input-pathname)))
         (initial-position (get-initial-position grid)))
    (unique-cells (patrol grid nil initial-position))))

(defun part-2 ()
  (let* ((grid (read-input (input-pathname)))
         (initial-position (get-initial-position grid))
         (unmodified-path (patrol grid nil initial-position))
         (answer nil))
    (dolist (obstacle (remove-duplicates (map 'list #'ocoord-coord unmodified-path) :test #'equal)
                      (length (remove-duplicates answer :test #'equal)))
      (unless (and obstacle
                   (equal obstacle (ocoord-coord initial-position)))
        (when (eq (patrol grid obstacle initial-position) :loop)
          (pushnew obstacle answer :test #'equal))))))

(defconstant +solution-1+ 5239)
(defconstant +solution-2+ 1753)
