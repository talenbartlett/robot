(in-package :robot)

(defparameter *memo-table* (make-hash-table :test 'equal))
(defparameter *pounce-list* (make-hash-table :test 'equal))

(defcommand memo :user
    "Send or receive a memo. !memo send <user> <message>. !memo receive"
  (lambda (message)
   (with-slots (connection source arguments received-time) message
     (let ((command-args (split "\\s+" (second arguments) :limit 4)))
       (cond ((string-equal (second command-args) "send") (let ((recipient (third command-args))
								(msg (fourth command-args)))
							    (if recipient
								(progn (pushnew (list source received-time msg)
										(gethash recipient *memo-table*))
								       (if (gethash recipient *pounce-list*)
									   (incf (gethash recipient *pounce-list*))
									   (setf (gethash recipient *pounce-list*) 1))
								       (say message "Memo sent."))
								(say message "Specify a recipient."))))
	     ((string-equal (second command-args) "receive") (let ((memo-list (gethash source *memo-table*)))
							       (if (null memo-list)
								   (privmsg connection source "You don't have any memos.")
								   (progn (dolist (memo (gethash source *memo-table*))
									    (multiple-value-bind (s m h date mo yr day) (decode-universal-time (second memo))
									      (declare (ignore _))
									      (privmsg connection source
										       (format nil "Sent on ~a:~a:~a (GMT), ~a/~a/~a by ~a: ~a" h m s mo day yr (first memo) (third memo))))) 
									  (remhash source *memo-table*)))))
	     (t (privmsg connection source (format nil "!memo {send <user> <message> | receive}"))))))))
