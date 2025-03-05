;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY23")

(defun get-input (input-pathname)
  (let ((neighbor-table (make-hash-table :test #'eql))
        (package (find-package "ADVENT2024/DAY23")))
    (iterate (((left right) (#2M(lambda (line) (values-list (str:split #\- line)))
                                (scan-file input-pathname #'read-line))))
      (let ((left*  (intern (string-upcase left)  package))
            (right* (intern (string-upcase right) package)))
        (push right* (gethash left* neighbor-table '()))
        (push left* (gethash right* neighbor-table '()))))
    neighbor-table))

(defun two-vertex-cliques (neighbor-table)
  (collect-append
   (mapping (((vertex neighbors) (scan-hash neighbor-table)))
     (mappend (lambda (neighbor)
                (when (string-lessp (symbol-name vertex) (symbol-name neighbor))
                  (list (list vertex neighbor))))
              neighbors))))

(defun part-1 ()
  (/ (count-if (lambda (clique)
                 (find-if (lambda (sym)
                            (char= #\T (char (symbol-name sym) 0)))
                          clique))
               (let ((neighbor-table (get-input (input-pathname))))
                 (mappend (lambda (clique)
                            (let ((left-neighbors (gethash (first clique) neighbor-table))
                                  (right-neighbors (gethash (second clique) neighbor-table)))
                              (map 'list (lambda (common-neighbor) (list* common-neighbor clique))
                                   (intersection left-neighbors right-neighbors))))
                          (two-vertex-cliques neighbor-table))))
     3))

(defun bron-kerbosch (graph-vertices clique more-vertices excluded-vertices)
  (if (and (null more-vertices) (null excluded-vertices))
      (list clique)
      (let iter ((answer '())
                 (excluded-vertices excluded-vertices)
                 (more-vertices more-vertices))
        (if (null more-vertices)
            answer
            (let* ((this-vertex (car more-vertices))
                   (more-vertices* (cdr more-vertices))
                   (neighbors (gethash this-vertex graph-vertices)))
              (iter (append (bron-kerbosch graph-vertices
                                           (adjoin this-vertex clique)
                                           (intersection more-vertices* neighbors)
                                           (intersection excluded-vertices neighbors))
                            answer)
                (adjoin this-vertex excluded-vertices)
                more-vertices*))))))

(defun maximal-cliques (graph-vertices)
  (bron-kerbosch graph-vertices '() (hash-table-keys graph-vertices) '()))

(defun part-2 ()
  (format
   nil "~{~a~^,~}"
   (sort
    (first
     (sort
      (maximal-cliques (get-input (input-pathname)))
      #'> :key #'length))
    #'string-lessp :key #'symbol-name)))

(defparameter +solution-1+ 1227)

(defparameter +solution-2+ "CL,DF,FT,IR,IY,NY,QP,RB,SH,SL,SW,WM,WY")
