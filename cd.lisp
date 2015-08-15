(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "title")
   (prompt-read "artist")
   (or (parse-integer (prompt-read "rating") :junk-allowed t) 0)
   (y-or-n-p "ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "More? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file(in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd)
       (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where-old (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
     (and
      (if title    (equal (getf cd :title)  title)  t)
      (if artist   (equal (getf cd :artist) artist) t)
      (if rating   (equal (getf cd :rating) rating) t)
      (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparision-list clauses))))

(defun set-values (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
  (if title (setf (getf cd :title) title))
  (if artist (setf (getf cd :artist) artist))
  (if rating (setf (getf cd :rating) rating))
  (if ripped-p (setf (getf cd :ripped) ripped))))

(defun update-cd (cd set-fn)
  (funcall set-fn cd)
  cd)

(defun update (select-fn set-fn)
  (setf *db*
	(mapcar
	 #'(lambda (cd)
	     (when (funcall select-fn cd)
	       (update-cd cd set-fn))
	     cd)
	 *db*)))

(defun delete-cds (select-fn)
  (setf *db*
	(remove-if select-fn *db*)))
	     
(defun make-comparision-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparision-list (fields)
  (loop while fields
       collecting (make-comparision-expr (pop fields) (pop fields))))
