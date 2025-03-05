;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY25")

(defun read-input (pathname)
  (let ((package (find-package "ADVENT2024/DAY25")))
    (with-open-file (stream pathname)
      (let iter ((line (read-line stream nil))
                 (accum '())
                 (locks '())
                 (keys '()))
        (if line
            (let ((char-list (map 'list (lambda (c) (intern (string c) package)) line)))
              (if (null char-list)
                  (let ((item (make-grid (length accum) (length (first accum))
                                         :initial-contents (reverse accum))))
                    (if (every (lambda (s) (eq s '\#)) (first accum))
                        (iter (read-line stream nil)
                              '()
                              locks
                              (cons item keys))
                        (iter (read-line stream nil)
                              '()
                              (cons item locks)
                              keys)))
                  (iter (read-line stream nil)
                        (cons char-list accum)
                        locks
                        keys)))
            (let ((item (make-grid (length accum) (length (first accum))
                                   :initial-contents (reverse accum))))
              (if (every (lambda (s) (eq s '\#)) (first accum))
                  (values (reverse locks) (reverse (cons item keys)))
                  (values (reverse (cons item locks)) (reverse keys)))))))))

(defun fits? (key lock)
  (collect-and (#m(lambda (k l)
                    (or (eql k '|.|) (eql l '|.|)))
                  (scan 'array key)
                  (scan 'array lock))))

(defun part-1 ()
  (multiple-value-bind (locks keys) (read-input (input-pathname))
    (count t (map-product #'fits? keys locks))))

(defun part-2 () 0)

(defparameter +solution-1+ 3291)
(defparameter +solution-2+ 0)
