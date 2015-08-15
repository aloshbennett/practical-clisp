(defvar *test-name* nil)

;(defun test+ ()
;  (let ((*test-name* 'test+))
;    (check (= (+ 2 3) 5)
;	   (= (+ 1 2) 2)
;	   (= (+ 3 3) 6))))

;(defun test* ()
;  (let ((*test-name* 'test*))
;    (check (= (* 2 3) 6)
;	   (= (* 1 2) 2))))

;(defun test-math ()
;  (let ((*test-name* 'test-math))
;  (combine-results
;    (test+)
;    (test*))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect
	    `(report-result ,f ',f))))

(defmacro with-my-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-my-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect
	      `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro deftest (test parameters forms)
  `(defun ,test ,parameters
     (let ((*test-name* (append *test-name* (list ',test))))
       (create-test-body ,forms))))

(defmacro create-test-body (forms)
  `(combine-results
     ,@(loop for f in forms collect
	    `(check ,f))))

(deftest test+ ()
  ((= (+ 2 3) 5)
   (= (+ 1 2) 2)
   (= (+ 3 3) 6)))

(deftest test* ()
  ((= (* 2 3) 6)
   (= (* 1 2) 2)))

(deftest test-math ()
  ((test+)
   (test*)))


