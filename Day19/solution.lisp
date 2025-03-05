;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY19")

(defun read-input (input-pathname)
  (let ((parsed
          (collect 'list
            (#M(lambda (line)
                 (map 'list #'str:trim (str:split #\, line)))
               (scan-file input-pathname #'read-line)))))
    (values (first parsed) (map 'list #'first (rest (rest parsed))))))

(defun can-make-sequence? (fragments sequence)
  (or (zerop (length sequence))
      (some
       (lambda (fragment)
         (multiple-value-bind (prefix? suffix)
             (starts-with-subseq fragment sequence :return-suffix t)
           (and prefix?
                (can-make-sequence? fragments suffix))))
       fragments)))

(defun test-1 ()
  (multiple-value-bind (fragments sequences) (read-input (sample-input-pathname))
    (count-if (lambda (sequence)
                  (can-make-sequence? fragments sequence))
              sequences)))

(defun part-1 ()
  (multiple-value-bind (fragments sequences) (read-input (input-pathname))
    (count-if (lambda (sequence)
                  (can-make-sequence? fragments sequence))
              sequences)))

(defparameter *count-solutions-cache* (make-hash-table :test 'equal))

(defun count-solutions (fragments sequence)
  (or (gethash (cons fragments sequence) *count-solutions-cache*)
      (let ((answer
             (if (zerop (length sequence))
                 1
                 (collect-sum
                  (#M(lambda (fragment)
                       (multiple-value-bind (prefix? suffix)
                           (starts-with-subseq fragment sequence :return-suffix t)
                         (if prefix?
                             (count-solutions fragments suffix)
                             0)))
                    (scan 'list fragments))))))
        (setf (gethash (cons fragments sequence) *count-solutions-cache*) answer)
        answer)))

(defun test-2 ()
  (multiple-value-bind (fragments sequences) (read-input (sample-input-pathname))
    (collect-sum
     (#m(lambda (sequence)
          (count-solutions fragments sequence))
        (scan 'list sequences)))))

(defun part-2 ()
  (multiple-value-bind (fragments sequences) (read-input (input-pathname))
    (collect-sum
     (#m(lambda (sequence)
          (count-solutions fragments sequence))
        (scan 'list sequences)))))

(defconstant +solution-1+ 355)
(defconstant +solution-2+ 732978410442050)
