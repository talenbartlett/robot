(in-package :robot)

;(<command> <auth level> <fn>)

(defparameter *command-table* (make-hash-table :test #'equal))

(defmacro defcommand (name regex auth help-string &body body)
  `(setf (gethash ,(write-to-string name) *command-table*)
	 (list ,regex ,auth ,help-string ,@body)))

(defcommand topic "^!topic" :user
  "Pick a topic at random from a stored list and change the topic to the selection."
  #'random-topic)

(defcommand quit "^!quit" :admin
  "Disconnect the bot from the server."
  #'quit-bot)

(defcommand join "^!join" :admin
  "Join the bot to a channel."
  (lambda (message)
    (let ((command-args (rest (split-whitespace (second (arguments message))))))
      (join (connection message)
	    (first command-args)))))

(defcommand register "^!register" :user
  "Register an account with the bot."
  #'register-user)

(defcommand login "^!login" :user
  "Log in to a bot account."
  #'login)

(defcommand logout "^!logout" :user
  "Log out of a bot account."
  #'logout)

(defcommand help "^!help" :user
  "Display the help message."
  (lambda (message)
    (with-slots (connection arguments) message
      (let ((command-args (rest (split-whitespace (second arguments)))))
	(if (zerop (length command-args))
	    (say message
		 (format nil "Available commands are: ~{~a~^ ~}~%!<command> to use."
			 (loop for k being the hash-keys of *command-table* collecting k)))
	    (say message
		 (format nil "~@[~a~]" (third (gethash
					       (string-upcase (first command-args))
					       *command-table*)))))))))

(defcommand base "^!base" :user
  "Convert a decimal number to another base."
  (lambda (message)
    (with-slots (connection source arguments) message
      (let ((command-args (rest (split-whitespace (second arguments)))))
	(when (= (length command-args) 2)
	    (let ((number (parse-integer (first command-args)
					 :junk-allowed t))
		  (new-base (parse-integer (second command-args)
					   :junk-allowed t)))
	      (say message (write-to-string number
					    :base (if (and (>= new-base 2)
							   (<= new-base 36))
						      new-base 10)))))))))

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

(defun quit-bot (message)
  (with-slots (connection) message
    (quit connection "Quitting.")))

(defun speak (message)
  (let ((split-msg (split-message message)))
    (when split-msg
      (cond ((= (random *message-chance*) (1- *message-chance*)) 
	     (privmsg (connection message) (destination message) 
		      (response-sentence (get-fragment split-msg))))))))

(defun speak-less (message)
  (unless (>= *message-chance* 64)
      (setf *message-chance* (* *message-chance* 2)))
  (privmsg (connection message) (destination message)
	   (format nil "Now speaking ~a percent of the time." (* 100 (/ 1 *message-chance*)))))

(defun speak-more (message)
  (unless (= *message-chance* 1) 
    (setf *message-chance* (/ *message-chance* 2)))
  (privmsg (connection message) (destination message) 
	   (format nil "Now speaking ~a percent of the time." (* 100 (/ 1 *message-chance*)))))

(defun stop-speaking (message)
  (setf *speaking* nil)
  (privmsg (connection message) (destination message) "Speaking disabled."))

(defun start-speaking (message)
  (setf *speaking* t)
  (privmsg (connection message) (destination message) "Speaking now."))
