(in-package :robot)

(defparameter *topics-table* (make-hash-table :test 'equal))

(defun last-topic (message)
  (with-slots (connection arguments) message
    (topic- connection
	    (destination message)
	    (format nil "~a" (gethash (destination message) *last-topic-table*)))))

(defun save-topic (channel topic)
  (pushnew topic (gethash channel *topics-table*) :test #'string-equal))

(defun random-topic (message)
  (with-slots (connection arguments) message
    (let* ((channel (find-channel connection
				  (first arguments)))
	   (topic-list (gethash (first arguments) *topics-table*)))
      (unless (null topic-list)
	(topic- connection channel (elt topic-list (random (length topic-list))))))))

(defun save-topic-file ()
  (save-table "topics" *topics-table*))

(defun read-topic-file ()
  (let ((table (read-table "topics")))
    (when table
      (setf *topics-table* (read-table "topics")))))
