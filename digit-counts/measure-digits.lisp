;; just some common lisp for getting digit counts of enormous numbers.
(defun seq-count-by (keyfun seq)
  "Count the items in SEQ, transformed to keys by KEYFUN. Return a hash-table"
  (reduce #'(lambda (hash item)
	      (incf (gethash (funcall keyfun item) hash 0))
	      hash)
	  seq
	  :initial-value (make-hash-table :size 10)))

(defun count-digits (number)
  "display counts for the digits in NUMBER"
  (maphash #'(lambda (k v)
	       (format t "Digit ~a: ~d~%" k v))
	   (seq-count-by #'identity (princ-to-string number))))

(defun every-= (hash)
  "Are all the values in hash-table `hash` =?"
  (apply #'=
	 (loop :for v :being :the :hash-values :of hash :collect v)))

;; let's look for large numbers that have the same number of every digit...
(loop for y from 100 upto 999
	thereis (loop for x from 7 upto 200
			thereis (and (every-=
				      (seq-count-by #'identity
						    (princ-to-string (expt y x))))
				     (list y x))))
;; didn't see any...
