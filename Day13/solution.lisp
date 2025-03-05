;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY13")

;; Cramers Rule to solve Ax + By = M, Cx + Dy = N
;; x = (MD - BN) / (AD - BC)
;; y = (AN - MC) / (AD - BC)

(defun cramers-rule (A B C D M N)
  (let ((det (- (* A D) (* B C))))
    (if (= det 0)
        nil
        (values (/ (- (* M D) (* B N)) det)
                (/ (- (* A N) (* M C)) det)))))

(defun puzzle (pathname &optional (conversion 0))
  (collect-sum
   (multiple-value-bind (line1 line2 line3 line4) (chunk 4 4 (catenate (scan-file pathname #'read-line)
                                                                       (scan 'list '(""))))
     (#M(lambda (line1 line2 line3)
          (cl-ppcre:register-groups-bind ((#'parse-integer ax) (#'parse-integer ay))
              ("Button A: X\\+(\\d+), Y\\+(\\d+)" line1)
            (cl-ppcre:register-groups-bind ((#'parse-integer bx) (#'parse-integer by))
                ("Button B: X\\+(\\d+), Y\\+(\\d+)" line2)
              (cl-ppcre:register-groups-bind ((#'parse-integer px) (#'parse-integer py))
                  ("Prize: X\\=(\\d+), Y\\=(\\d+)" line3)
                (multiple-value-bind (x y) (cramers-rule ax bx
                                                         ay by
                                                         (+ px conversion)
                                                         (+ py conversion))
                  (if (and x y (>= x 0) (>= y 0) (integerp x) (integerp y))
                      (+ (* x 3) y)
                      0))))))
        line1 line2 line3))))

(defun part-1 ()
  (puzzle (input-pathname)))

(defun part-2 ()
  (puzzle (input-pathname) (expt 10 13)))

(defconstant +solution-1+ 38839)
(defconstant +solution-2+ 75200131617108)
