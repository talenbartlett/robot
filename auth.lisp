(in-package :robot)

(defparameter *user-table* (make-hash-table :test 'equal))
(defparameter *active-users* (make-hash-table :test 'equal)) ; (user irc-user host)

(defun register-user (message &key (privileges :user))
  (with-slots (connection source user host arguments) message
    (let ((hashed-password (hash-string-password (second arguments)))
	  (user-info (gethash source *user-table*)))
      (if (not user-info)
	  (progn
	    (setf (gethash source *user-table*) (list hashed-password privileges))
	    (privmsg connection source "[AUTH] Registration succeeded."))
	  (privmsg connection source "[AUTH] Registration failed. It is possible this nickname is already registered.")))))

(defun hash-string-password (password)
  (pbkdf2-hash-password-to-combined-string
   (ascii-string-to-byte-array password)))

(defun login (message)
  (with-slots (connection source user host arguments) message
    (let ((user-info (gethash source *user-table*))
	  (hashed-password (hash-string-password (second arguments))))
     (if (and
	  (not (null user-info))
	  (pbkdf2-check-password (ascii-string-to-byte-array password)
				 (first user-info)))
	 (progn (pushnew (list user host)
			 (gethash source *active-users*)
			 :test #'string-equal)
		(privmsg connection source (format nil "[AUTH] Logged in as user: ~a." source)))
	 (privmsg connection source (format nil "[AUTH] Could not login user: ~a" source))))))

(defun logout (message)
  (with-slots (connection source user host) message
    (let* ((active-user-info (gethash source *active-users*))
	   (irc-user (first active-user-info))
	   (irc-host (second active-user-info)))
      (if (and (not (null active-user-info))
	       (and (string= user irc-user)
		    (string= host irc-host)))
	  (progn
	    (remhash source *active-users*)
	    (privmsg connection source "[AUTH] Logged out."))
	  (privmsg connection source "[AUTH] Access Denied.")))))

(defun authorized-p (nick)
  (eq (second (gethash nick *user-table*)) :admin))

(defun read-user-file ()
  (let ((table (read-table "users")))
    (when table
     (setf *user-table* (read-table "users")))))

(defun save-user-file ()
  (save-table "users" *user-table*))
