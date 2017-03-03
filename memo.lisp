(in-package :robot)

(defparameter *memo-table* (make-hash-table :test 'equal))

(defcommand memo "^!memo" :user
    "Send or receive a memo. !memo send <user> <message>. !memo receive"
  (with-slots (connection source arguments)
      (let ((command-args (command-arguments arguments)))
	(cond ((string= (first command-args) "send") (pushnew (list source (third command-args))
							      (gethash (second commands-args) *memo-table*))
	       (string= (first command-args) "receive") (when (logged-in-p source)
							  (privmsg connection source
								   (format nil "~{~a~^~%~}" (gethash source *memo-table*)))
							  (remhash source *memo-table*)))))))
