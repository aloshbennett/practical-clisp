(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(defun do-prime-fun (number)
  (do ((p (next-prime 0) (next-prime (1+ p))))
      ((> p number))
    (format t "~d " p)))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
    
(defmacro do-primes-1 ((var start end) &body body)
  (let ((ending-value-var (gensym)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(,ending-value-var ,end))
       ((> ,var ,ending-value-var))
     ,@body)))

(defmacro with-my-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-2 ((var start end) &body body)
  (with-my-gensyms (ending-value-var)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-var ,end))
	 ((> ,var ,ending-value-var))
       ,@body)))

(defun print-primes (number)
  (do-primes-2 (num 0 number)
    (format t "~d " num)))


