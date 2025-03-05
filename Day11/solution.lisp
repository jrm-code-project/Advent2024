;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY11")

(defun read-input (input-pathname)
  (collect 'list (scan-file input-pathname)))

(defun digit-count (stone)
  (1+ (integer-log stone 10)))

(defun split-stone (stone)
  (let ((divisor (expt 10 (/ (digit-count stone) 2))))
    (list (floor stone divisor)
          (mod stone divisor))))

(defun apply-rules (stone)
  (cond ((zerop stone) (list 1))
        ((evenp (digit-count stone)) (split-stone stone))
        (t (list (* stone 2024)))))

(defun coalesce (stone-alist)
  (let ((table (make-hash-table :test 'eql)))
    (dolist (stone-entry stone-alist table)
      (incf (gethash (car stone-entry) table 0) (cdr stone-entry)))))

(defun blink (stone-table)
  (coalesce
   (multiple-value-bind (stone-values stone-counts) (scan-hash stone-table)
     (collect-append
      (map-fn 'list
              (lambda (stone-value stone-count)
                (map 'list
                     (lambda (new-stone) (cons new-stone stone-count))
                     (apply-rules stone-value)))
              stone-values
              stone-counts)))))

(defun initial-stone-table (initial-stones)
  (coalesce (map 'list (lambda (stone) (cons stone 1)) initial-stones)))

(defun blink-n (initial-stones n)
  (do ((stone-table (initial-stone-table initial-stones) (blink stone-table))
       (generation 0 (1+ generation)))
     ((>= generation n) (collect-sum (multiple-value-bind (stones counts) (scan-hash stone-table) counts)))))

(defun part-1 ()
  (blink-n (read-input (input-pathname)) 25))

(defun part-2 ()
  (blink-n (read-input (input-pathname)) 75))

(defconstant +solution-1+ 228668)
(defconstant +solution-2+ 270673834779359)
