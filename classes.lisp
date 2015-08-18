(defclass person ()
  ((name :initarg :name)
   (age :initarg :age)
   mood))

(defmethod print-object ((p person) stream)
  (format stream  "#<person: name=~a, age=~a, mood=~a>"
	  (slot-value p 'name)
	  (slot-value p 'age)
	  (slot-value p 'mood)))

(defun create-person (name age)
  (let ((p (make-instance 'person)))
    (setf (slot-value p 'name) name)
    (setf (slot-value p 'age) age)
    p))

(defun create-person1 (name age)
  (make-instance 'person :name name :age age))

(defmethod initialize-instance :after ((p person) &rest initargs)
  (setf (slot-value p 'mood) "happy"))

(defparameter *me* (make-instance 'person))
(setf (slot-value *me* 'name) "alosh")
(setf (slot-value *me* 'age) 33)

