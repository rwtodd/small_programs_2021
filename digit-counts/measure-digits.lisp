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
