(in-package :robot)

;(<command> <auth level> <fn>)

(defparameter *command-table* (make-hash-table :test #'equalp))

(defmacro defcommand (name auth &optional (help-string "") &body body)
  `(setf (gethash (format nil "!~a" ',name) *command-table*)
	 (list ,auth ,help-string ,@body)))

(defcommand rename :admin
    "Rename the bot. !rename <nick>"
  (lambda (message)
    (with-slots (connection arguments) message
      (let ((command-args (command-arguments arguments)))
	(if (null command-args)
	    (nick connection (make-nick))
	    (nick connection
		  (first command-args)))))))

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
			    when (eq (first v) :user)
			    collect (string-left-trim "!" k))))
	    (say message
		 (format nil "~@[~a~]" (second (gethash
					       (format nil "!~a" (first command-args))
					       *command-table*)))))))))

(defcommand reverse :user
    "Reverse the provided string."
  (lambda (message)
    (with-slots (connection arguments) message
      (let* ((command-args (command-arguments arguments))
	     (reversed-args (reverse (mapcar 'reverse command-args))))
	(say message
	     (format nil "~{~a~^ ~}" reversed-args))))))

(defun search-command-table (message)
  (with-slots (connection source arguments) message
    (let* ((user-input (second arguments))
	   (action (gethash (first (split-whitespace user-input)) *command-table*)))
      (if (null action)
	  nil
	  (if (eq (first action) :user)
	      (funcall (third action) message)
	      (if (authorized-p source)
		  (funcall (third action) message)
		  (privmsg connection source "You are not authorized.")))))))
