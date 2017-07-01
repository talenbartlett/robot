;;;; robot.lisp

(in-package #:robot)

(defun threaded-connection (ip &key (nick (make-nick)) (port :default) (ssl :none) (channels nil))
  (make-thread (lambda () (make-bot ip :nick nick :port port :ssl ssl :channels channels))
	       :name (format nil "~a-thread" nick)))

(defun make-bot (ip &key (nick (make-nick)) (port :default) (ssl :none) (channels nil) (logging-stream nil))
  
  (let ((connection (connect :server ip
			     :logging-stream logging-stream
			     :nickname nick
			     :port port
			     :connection-security ssl
			     :username "bot")))
    
    (dolist (channel channels)
      (join connection channel))
    
    (read-user-file)    
    (read-topic-file)
    (read-dictionary)
    (load-custom-hooks connection)    
    (read-message-loop connection)
    (save-dictionary)
    (save-topic-file)
    (save-user-file)))
