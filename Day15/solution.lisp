;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY15")

(defun decode-cell (string package wide?)
  (if wide?
      (cond ((equal string "#") (list '|#| '|#|))
            ((equal string "O") (list '|[| '|]|))
            ((equal string ".") (list '|.| '|.|))
            ((equal string "@") (list '|@| '|.|))
            (t (error "Unknown cell ~a" string)))
      (list (intern (string-upcase string) package))))

(defun decode-move (move)
  (cond ((equal move "<") +west+)
        ((equal move "^") +north+)
        ((equal move ">") +east+)
        ((equal move "v") +south+)
        (t (error "Unknown move ~a" move))))

(defun read-input (input-pathname &optional (wide? nil))
  (multiple-value-bind (blanks grids moves)
      (#3M(lambda (line)
            (cl-ppcre:register-groups-bind (blank grid move)
                ("(^$)|([#.O@]+)|([><v^]+)" line)
              (values blank grid move)))
          (scan-file input-pathname #'read-line))
    (let ((blank-lines (collect 'list (choose blanks)))
          (grid-lines  (collect 'list
                         (#M(lambda (line)
                              (collect-append (#Mdecode-cell
                                               (#Mstring (scan 'string line))
                                               (series (find-package "ADVENT2024/DAY15"))
                                               (series wide?))))
                            (choose grids))))
          (move-list (collect-append (#M(lambda (line)
                                          (collect 'list (#Mdecode-move
                                                           (#Mstring (scan 'string line)))))
                                        (choose moves)))))
      (declare (ignore blank-lines))
      (values (make-grid (length grid-lines) (length (first grid-lines)) :initial-contents grid-lines)
              move-list))))

(defun can-move-to? (grid coord delta)
  "True if location on grid at coord is empty, or item at location can move in direction."
  (and (on-grid? grid coord)
       (or (eql (grid-ref grid coord) '|.|)
           (can-move? grid coord delta))))

(defun can-move? (grid coord delta)
  "True if item on grid at coord can move in direction."
  (and (on-grid? grid coord)
       (ecase (grid-ref grid coord)
         (|.| (error "No item at coord."))
         (|#| nil)
         (|@| (let ((target (2v+ coord delta)))
                (can-move-to? grid target delta)))
         (|O| (let ((target (2v+ coord delta)))
                (can-move-to? grid target delta)))
         (|[| (if (or (equal delta +north+)
                      (equal delta +south+))
                  (let ((target1 (2v+ coord delta))
                        (target2 (2v+ (2v+ coord delta) +east+)))
                    (and (can-move-to? grid target1 delta)
                         (can-move-to? grid target2 delta)))
                  (let ((target (2v+ coord delta)))
                    (can-move-to? grid target delta))))
         (|]| (if (or (equal delta +north+)
                      (equal delta +south+))
                  (let ((target1 (2v+ coord delta))
                        (target2 (2v+ (2v+ coord delta) +west+)))
                    (and (can-move-to? grid target1 delta)
                         (can-move-to? grid target2 delta)))
                  (let ((target (2v+ coord delta)))
                    (can-move-to? grid target delta)))))))

(defun move! (grid coord delta)
  "Move item on grid at coord in direction delta."
  (when (can-move? grid coord delta)
    (ecase (grid-ref grid coord)
      (|.| (error "Cannot move empty locations."))
      (|#| (error "Cannot move walls."))
      (|@| (let ((target (2v+ coord delta)))
             (unless (eql (grid-ref grid target) '|.|)
               (move! grid target delta))
             (setf (grid-ref grid target) '|@|
                   (grid-ref grid coord) '|.|)
             target))
      
      (|O| (let ((target (2v+ coord delta)))
             (unless (eql (grid-ref grid target) '|.|)
               (move! grid target delta))
             (setf (grid-ref grid target) '|O|
                   (grid-ref grid coord) '|.|)
             target))

      (|[| (let* ((targetl (2v+ coord delta))
                  (targetr (2v+ targetl +east+)))
             (unless (or (eql delta +east+)
                         (eql (grid-ref grid targetl) '|.|))
               (move! grid targetl delta))
             (unless (or (eql delta +west+)
                         (eql (grid-ref grid targetr) '|.|))
               (move! grid targetr delta))
             (setf (grid-ref grid targetl) '|[|
                   (grid-ref grid targetr) '|]|)
             (unless (eql delta +east+)
               (setf (grid-ref grid (2v+ coord +east+)) '|.|))
             (unless (eql delta +west+)
               (setf (grid-ref grid coord) '|.|))
             targetl))

      (|]| (let* ((targetr (2v+ coord delta))
                  (targetl (2v+ targetr +west+)))
             (unless (or (eql delta +east+)
                         (eql (grid-ref grid targetl) '|.|))
               (move! grid targetl delta))
             (unless (or (eql delta +west+)
                         (eql (grid-ref grid targetr) '|.|))
               (move! grid targetr delta))
             (setf (grid-ref grid targetl) '|[|
                   (grid-ref grid targetr) '|]|)
             (unless (eql delta +east+)
               (setf (grid-ref grid coord) '|.|))
             (unless (eql delta +west+)
               (setf (grid-ref grid (2v+ coord +west+)) '|.|))
             targetr)))))

(defun find-robot (grid)
  (collect-first
   (choose
    (mapping (((coord item) (scan-grid grid)))
      (when (eql item '|@|)
        coord)))))

(defun score-map (grid)
  (collect-sum
   (mapping (((coord item) (scan-grid grid)))
     (if (or (eql item '|O|)
             (eql item '|[|))
         (+ (* (row coord) 100) (column coord))
         0))))

(defun show-grid (grid)
  (dotimes (row (grid-height grid))
    (dotimes (column (grid-width grid))
      (format t "~a" (grid-ref grid (coord column row))))
    (format t "~%")))

(defun puzzle (input-file wide?)
  (multiple-value-bind (grid moves) (read-input input-file wide?)
    (fold-left (lambda (robot move)
                 (if (can-move? grid robot move)
                     (move! grid robot move)
                     robot))
               (find-robot grid)
               moves)
    (score-map grid)))

(defun part-1 ()
  (puzzle (input-pathname) nil))

(defun part-2 ()
  (puzzle (input-pathname) t))

;(puzzle (sample-input-pathname 1) nil)
; ########
; #....OO#
; ##.....#
; #.....O#
; #.#O@..#
; #...O..#
; #...O..#
; ########
;  => 2028 (11 bits, #x7EC)
;(puzzle (sample-input-pathname 1) t)
; ################
; ##........[]..##
; ####....[]....##
; ##......[]....##
; ##..##...]....##
; ##....@.......##
; ##......[]....##
; ################
;  => 1234 (11 bits, #x4D2)
;(puzzle (sample-input-pathname) nil)
; ##########
; #.O.O.OOO#
; #........#
; #OO......#
; #OO@.....#
; #O#.....O#
; #O.....OO#
; #O.....OO#
; #OO....OO#
; ##########
;  => 10092 (14 bits, #x276C)
;(puzzle (sample-input-pathname) t)
;(puzzle (input-pathname) nil)
;(puzzle (input-pathname) t)

(defconstant +solution-1+ 1552463)
(defconstant +solution-2+ 1554058)
