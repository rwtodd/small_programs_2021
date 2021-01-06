;; just some common lisp for getting digit counts of enormous numbers.

(defun group-digits (n)
  "Generate a hash table mapping digits in `n` to their counts in `n`."
  (let ((hash (make-hash-table)))
    (map 'nil
	 #'(lambda (ch) (setf (gethash ch hash) (1+ (gethash ch hash 0))))
	 (princ-to-string n))
    hash))

(defun print-hash (h)
  "Just display the keys and values of the hash"
  (maphash #'(lambda (k v) (format t "~A  ~A~%" k v)) h))

(defun every-= (hash)
  "Are all the values in hash-table `hash` =?"
  (apply #'=
	 (loop :for v :being :the :hash-values :of hash :collect v)))

;; let's look for large numbers that have the same number of every digit...
(loop for y from 6001 upto 8000
	thereis (loop for x from 7 upto 200
			thereis (and (every-= (group-digits (expt y x)))
				     (list y x))))
;; didn't see any...
