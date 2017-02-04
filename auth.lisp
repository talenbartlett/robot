(in-package :robot)

(defparameter *user-table* (make-hash-table :test 'equal))
(defparameter *active-users* (make-hash-table :test 'equal))

(defun register-user (message)
  (with-slots (connection source user host arguments) message
    (let* ((user-input-list (split-whitespace (second arguments)))
	   (hashed-password (hash-string-password (second user-input-list)))
	   (user-info (gethash source *user-table*)))
      (if (not user-info)
	  (progn
	    (setf (gethash source *user-table*) (list hashed-password
						      (if (zerop (hash-table-count *user-table*))
							  :admin
							  :user)))
	    (save-user-file)
	    (privmsg connection source "[AUTH] Registration succeeded. Log in with !login <password>."))
	  (privmsg connection source "[AUTH] Registration failed. It is possible this nickname is already registered.")))))

(defun hash-string-password (password)
  (when password
    (pbkdf2-hash-password-to-combined-string
     (ascii-string-to-byte-array password))))

(defun login (message)
  (with-slots (connection source user host arguments) message
    (let* ((user-input-list (split-whitespace (second arguments)))
	   (user-info (gethash source *user-table*))
	   (bot-pass (second user-input-list))
	   (hashed-password (hash-string-password bot-pass)))
     (if (and
	  user-info
	  bot-pass
	  (pbkdf2-check-password (ascii-string-to-byte-array bot-pass)
				 (first user-info)))
	 (progn (setf (gethash source *active-users*) (list user host))
		(privmsg connection source (format nil "[AUTH] Logged in as user: ~a." source)))
	 (privmsg connection source (format nil "[AUTH] Could not log in user: ~a" source))))))

(defun logout (message)
  (with-slots (connection source user host) message
    (let* ((active-user-info (gethash source *active-users*))
	   (irc-user (first active-user-info))
	   (irc-host (second active-user-info)))
      (if (and active-user-info
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
