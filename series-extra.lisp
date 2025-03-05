(in-package "SERIES")

(export '(
          collect-hash-push
          collect-hash-push-except
          ))

(defS collect-hash-push (keys values &rest option-plist)
  "(collect-hash-push keys values :test :size :rehash-size :rehash-threshold)

Combines a series of keys and a series of values together into a hash
table that maps each key to a list of values.  The keyword arguments
specify the attributes of the hash table to be produced;  they are
used as arguments to MAKE-HASH-TABLE."
  (fragl ((keys t) (values t) (option-plist))
	 ((table))
	 ((table t (apply #'make-hash-table option-plist)))
	 ()
         ()
         ((push values (gethash keys table))) () () nil)
  :optimizer
  (apply-literal-frag
   (list '(((keys t) (values t) (table)) ((table)) () ()
           () ((push values (gethash keys table))) () () nil)
         keys values `(make-hash-table ,@ option-plist)))
  :trigger t)

(defS collect-hash-push-except (keys values exceptions &rest option-plist)
  "(collect-hash-push-except keys values :element-test :rehash-size :rehash-threshold :size :test)

Combines a series of keys and a series of values together into a hash
table that maps each key to a list of values.  The keyword arguments
specify the attributes of the hash table to be produced;  they are
used as arguments to MAKE-HASH-TABLE."
  (fragl ((keys t) (values t) (exceptions) (option-plist))
	 ((table))
	 ((table t (apply #'make-hash-table (alexandria:remove-from-plist option-plist :element-test)))
          (element-test t (getf option-plist :element-test 'eql)))
	 ()
         ()
         ((unless (member keys exceptions :test element-test)
            (push values (gethash keys table)))) () () nil)
  :optimizer
  (apply-literal-frag
   (list '(((keys t) (values t) (exceptions) (table) (element-test)) ((table)) () ()
           () ((unless (member keys exceptions :test element-test)
                 (push values (gethash keys table)))) () () nil)
         keys values exceptions
         `(make-hash-table ,@(alexandria:remove-from-plist option-plist :element-test))
         `,(getf option-plist :element-test '(function eql))))
  :trigger t)
