;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY22")

(defun next-pseudorandom (pseudorandom)
  (check-type pseudorandom (integer 0 (16777216)))
  (macrolet ((mix (a b) `(logxor ,a ,b))
             (prune (x) `(mod ,x 16777216)))
    (let* ((s1 (prune (mix (* pseudorandom 64) pseudorandom)))
           (s2 (prune (mix (floor s1 32) s1)))
           (s3 (prune (mix (* s2 2048) s2))))
      s3)))

(defun scan-pseudorandom (seed)
  (declare (optimizable-series-function))
  (scan-fn '(integer 0 (16777216))
           (lambda () seed)
           #'next-pseudorandom))

(defun nth-pseudorandom (seed n)
  (collect-nth n (scan-pseudorandom seed)))

(defun test-1 ()
  (collect-sum (#Mnth-pseudorandom (scan-file (sample-input-pathname)) (series 2000))))

(defun part-1 ()
  (collect-sum (#Mnth-pseudorandom (scan-file (input-pathname)) (series 2000))))

(defun scan-prices (initial-value)
  (declare (optimizable-series-function))
  (#Mmod (scan-pseudorandom initial-value) (series 10)))

(defun scan-monkeys (input-pathname)
  (declare (optimizable-series-function 2))
  (cotruncate (scan-range :from 0)
              (scan-file input-pathname)))

(defun deltas (series)
  (declare (optimizable-series-function)
           (off-line-port series))
  (mapping (((before after) (chunk 2 1 series)))
           (- after before)))

(defun price-trends (price-series)
  (declare (optimizable-series-function))
  (mapping (((d1 d2 d3 d4) (chunk 4 1 (price-deltas price-series))))
           (list d1 d2 d3 d4)))

(defun add-trend-info! (table monkeyid seed)
  (iterate ((count (scan-range :from 4 :below 2001))
            (trend (price-trends (scan-prices seed)))
            (price (subseries (scan-prices seed) 4)))
    (declare (ignore count))
    (unless (assoc monkeyid (gethash trend table '()))
      (push (cons monkeyid price) (gethash trend table '())))))

(defun trend-table-maximum (table)
  (let ((best-score 0)
        (best-key nil))
    (maphash (lambda (key value)
               (let ((score (reduce #'+ (map 'list #'cdr value))))
                 (when (> score best-score)
                   (setq best-key key)
                   (setq best-score score))))
             table)
    (values best-key best-score)))

(defun part-2 ()
  (multiple-value-bind (best-key best-value)
      (let ((table (make-hash-table :test #'equal)))
        (iterate (((monkeyid seed) (scan-monkeys (input-pathname))))
          (add-trend-info! table monkeyid seed))
        (trend-table-maximum table))
    (declare (ignore best-key))
    best-value))

(defparameter +solution-1+ 13185239446)
(defparameter +solution-2+ 1501)
