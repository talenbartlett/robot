(in-package :robot)

;(<command> <auth level> <fn>)

(defparameter *command-table* (make-hash-table :test #'equal))

(defmacro defcommand (name auth &optional (help-string "") &body body)
  `(setf (gethash ,(write-to-string name) *command-table*)
	 (list
	  (concatenate 'string "^!" (string-downcase (write-to-string ',name)))
	  ,auth ,help-string ,@body)))

(defcommand rename :admin
    "Rename the bot. !rename <nick>"
  (lambda (message)
    (with-slots (connection arguments) message
      (let ((command-args (command-arguments arguments)))
	(nick connection
	      (first command-args))))))

(defcommand quit :admin
    "Disconnect the bot from the server."
  (lambda (message)
   (with-slots (connection) message
     (quit connection "Quitting."))))

(defcommand join :admin
    "Join the bot to a channel. !join <channel>"
  (lambda (message)
    (with-slots (arguments) message
      (let ((command-args (command-arguments arguments)))
	(join (connection message)
	      (first command-args))))))

(defcommand help :user
    "Display the help message."
  (lambda (message)
    (with-slots (connection arguments) message
      (let ((command-args (rest (split-whitespace (second arguments)))))
	(if (zerop (length command-args))
	    (say message
		 (format nil "Available user commands are: ~{~a~^ ~}"
			 (loop for k being the hash-keys of *command-table*
			    using (hash-value v)
			    when (eq (second v) :user)
			    collect k)))
	    (say message
		 (format nil "~@[~a~]" (third (gethash
					       (string-upcase (first command-args))
					       *command-table*)))))))))

(defun search-command-table (message)
  (with-slots (connection source) message
    (let ((message-str (message-string message)))
      (loop for v being the hash-values of *command-table* do
	   (if (scan (first v) message-str)
	       (if (eq (second v) :user)
		   (funcall (fourth v) message)
		   (if (authorized-p source)
		       (funcall (fourth v) message)
		       (privmsg connection source "You are not authorized."))))))))

(defun say (message text)
  (privmsg (connection message)
	   (destination message)
	   text))
