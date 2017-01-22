(in-package :robot)

(defun search-command (message)
  (let ((message-str (message-string message)))
      (cond ((cl-ppcre:scan "^!stop" message-str) (stop-speaking message))
	    ((cl-ppcre:scan "^!speak" message-str) (start-speaking message))
	    ((cl-ppcre:scan "^!quieter" message-str) (speak-less message))
	    ((cl-ppcre:scan "^!louder" message-str) (speak-more message))
	    ((cl-ppcre:scan "^!topic" message-str) (random-topic message))
	    ((cl-ppcre:scan "^!quit" message-str) (quit-bot message))
	    ((cl-ppcre:scan "^!join" message-str) (join (connection message) (second (split-whitespace message-str))))
	    ((cl-ppcre:scan "^!register" message-str) (register-user message))
	    ((cl-ppcre:scan "^!login" message-str) (login message))
	    ((cl-ppcre:scan "^!logout" message-str) (logout message)))))


#|
(defmacro define-command (command command-trigger &body command-fn))

(define-command stop "^!stop" message (stop-speaking message))
(define-command speak "^!speak" message (stop-speaking message))
(define-command quieter "^!quieter" message (speak-less message))
(define-command louder "^!louder" message (speak-more message))
|#

(defun say (message text)
  (privmsg (connection message)
	   (destination message)
	   text))

(defun quit-bot (message)
  (with-slots (connection source user host) message
    (let* ((active-user-info (gethash source *active-users*))
	   (irc-user (first active-user-info))
	   (irc-host (second active-user-info)))
      (if (and (not (null active-user-info))
		 (and (string= user irc-user)
		      (string= host irc-host))
		 (eq (second (gethash source *user-table*)) :admin))
	(quit connection "Quitting.")
	(say message "Access Denied.")))))

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
