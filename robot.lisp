;;;; robot.lisp

(in-package #:robot)

(defparameter *nick-prefixes* (loop for a from (char-code #\A) to (char-code #\z)
				 collect (code-char a)))

(defvar *message-chance* 1)
(defvar *speaking* nil)
(defvar *train* nil)

(defun threaded-connection (ip &key (nick (make-nick)) (port :default) (ssl :none) (channels nil))
  (make-thread (lambda () (make-bot ip :port port :ssl ssl :channels channels))
	       :name (format nil "~a-thread" nick)))

(defun make-bot (ip &key (nick (make-nick)) (port :default) (ssl :none) (channels nil) (logging-stream nil))
  
  (let ((connection (connect :server ip
			     :logging-stream logging-stream
			     :nickname nick
			     :port port
			     :connection-security ssl
			     :username nick)))
    
    (dolist (channel channels)
      (join connection channel))

    (read-user-file)    
    (read-topic-file)
    (load-custom-hooks connection)    
    (read-message-loop connection)
    (save-topic-file)
    (save-user-file)))
