;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY9")

(defun read-input (input-file)
  (with-open-file (stream input-file :direction :input)
    (read-line stream nil nil)))

(defun read-layout (input-file)
  (let* ((line             (read-input input-file))
         (file-count       (/ (1+ (length line)) 2))
         (files            (make-simple-vector file-count))
         (free-block-count (1- file-count))
         (free-blocks      (make-simple-vector free-block-count)))
    (do ((input-index      0 (1+ input-index))
         (file-flag        t (not file-flag))
         (memory-loc       0 (+ memory-loc (char->decimal (schar line input-index))))
         (file-id          0 (if file-flag
                                 (1+ file-id)
                                 file-id))
         (free-block-index 0 (if file-flag
                                 free-block-index
                                 (1+ free-block-index))))
        ((>= input-index (length line)) (values files free-blocks))
      (if file-flag
          (setf (svref files file-id)
                (cons memory-loc (char->decimal (schar line input-index))))
          (setf (svref free-blocks free-block-index)
                (cons memory-loc (make-simple-vector (char->decimal (schar line input-index)) :initial-element nil)))))))

(defun files-checksum (files)
  (collect-sum
    (#M(lambda (file-id)
         (let ((file-record (svref files file-id)))
           (collect-sum
             (#M(lambda (i)
                  (* file-id (+ (car file-record) i)))
              (scan-range :below (cdr file-record))))))
        (scan-range :below (length files)))))

(defun freelist-checksum (freelist)
  (collect-sum
   (#M(lambda (freelist-record)
        (let* ((segment-base (car freelist-record))
               (segment (cdr freelist-record)))
         (collect-sum
           (#M(lambda (offset-in-segment)
                (let ((file-id (or (svref segment offset-in-segment) 0)))
                  (* file-id (+ segment-base offset-in-segment))))
            (scan-range :below (length segment))))))
    (scan 'vector freelist))))

(defun filesystem-checksum (files freelist)
  (+ (files-checksum files)
     (freelist-checksum freelist)))

(defun source-block (files)
  (do ((file-id (1- (length files)) (1- file-id)))
      ((not (zerop (cdr (svref files file-id))))
       (values (+ (car (svref files file-id)) (cdr (svref files file-id)))
               (svref files file-id)
               file-id))))

(defun target-block (freelist)
  (let* ((free-segment (find-if (lambda (segment)
                                  (position nil (cdr segment)))
                                freelist))
         (offset (position nil (cdr free-segment))))
    (values (+ (car free-segment) offset)
            (cdr free-segment)
            offset)))

(defun move-block! (files freelist)
  (multiple-value-bind (source-block source-record file-id) (source-block files)
    (multiple-value-bind (target-block target-segment target-offset) (target-block freelist)
      (when (< target-block source-block)
        (decf (cdr source-record))
        (setf (svref target-segment target-offset) file-id)
        t))))

(defun defrag1! (files freelist)
  (when (move-block! files freelist)
    (defrag1! files freelist)))

(defun puzzle (input-pathname defrag)
  (multiple-value-bind (files freelist) (read-layout input-pathname)
    (funcall defrag files freelist)
    (filesystem-checksum files freelist)))

(defun part-1 ()
  (puzzle (input-pathname) #'defrag1!))

(defun defrag2-target (file freelist)
  (collect-first
   (choose-if
    (lambda (free-record)
      (and (< (car free-record) (car file))
           (<= (cdr file) (count nil (cdr free-record)))))
    (scan 'vector freelist))))

(defun defrag2! (files freelist)
  (do ((file-id (1- (length files)) (- file-id 1)))
      ((zerop file-id))
    (let* ((file (svref files file-id))
           (target (defrag2-target file freelist)))
      (when target
        (let* ((start (position nil (cdr target)))
               (end (+ start (cdr file))))
          (fill (cdr target) file-id :start start :end end)
          (setf (cdr file) 0))))))

(defun part-2 ()
  (puzzle (input-pathname) #'defrag2!))

(defconstant +solution-1+ 6258319840548)
(defconstant +solution-2+ 6286182965311)
