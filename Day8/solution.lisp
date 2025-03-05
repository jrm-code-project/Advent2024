;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY8")

(defun read-grid (pathname)
  (read-file-into-grid (char-interner #'identity (find-package "ADVENT2024/DAY8")) pathname))

(defun grid->antenna-list (grid)
  (hash-table-alist (invert-grid grid '|.|)))

(defun antinode-answer (antinode-half-answer)
  (lambda (grid left right)
    (append (funcall antinode-half-answer grid left right)
            (funcall antinode-half-answer grid right left))))

(defun antinode-list (antinode-answer)
  (lambda (grid node-list)
    (map-pairs (lambda (left right)
                 (funcall antinode-answer grid left right))
               node-list)))

(defun antinodes (antinode-list-generator)
  (lambda (grid node-list)
    (fold-left #'append nil
               (funcall antinode-list-generator grid node-list))))

(defun puzzle (grid antinode-generator)
  (length
   (remove-duplicates
    (fold-left (lambda (antinodes entry)
                 (append antinodes (funcall antinode-generator grid (cdr entry))))
               nil
               (grid->antenna-list grid))
    :test #'equal)))

(defun antinode (grid left right)
  (let* ((delta (2v- right left))
         (antinode (2v- left delta)))
    (when (on-grid? grid antinode)
      (list antinode))))

(defun part-1 ()
  (puzzle (read-grid (input-pathname))
          (antinodes (antinode-list (antinode-answer #'antinode)))))

(defun resonant-antinodes (grid left right)
  (let* ((delta (2v- right left)))
    (do ((antinode left (2v- antinode delta))
         (answer '() (cons antinode answer)))
        ((not (on-grid? grid antinode)) answer))))

(defun part-2 ()
  (puzzle (read-grid (input-pathname))
          (antinodes (antinode-list (antinode-answer #'resonant-antinodes)))))

(defconstant +solution-1+ 271)
(defconstant +solution-2+ 994)
