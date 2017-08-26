(in-package :robot)

(defcommand metar :user
    "Download and display latest METAR. !metar <station id>"
  (let ((last-downloaded 0)
	(data))
    (lambda (message)
      (with-slots (arguments) message
	(let ((command-args (command-arguments arguments))
	      (time-since-last-download (abs (- (get-universal-time) last-downloaded))))
	  (when (or (> time-since-last-download 3600)
		    (null data))
	    (print "NOTE: Downloading METAR data!")
	    (setf data (get-metar)
		  last-downloaded (get-universal-time)))
	  (loop for table in (gethash "features" data)
	     when (string-equal (gethash "id" (gethash "properties" table))
				(first command-args))
	     do (let ((properties (gethash "properties" table)))
                  (say message (format nil "METAR for: ~a (~a)" (gethash "site" properties) (gethash "id" properties)))
                  (say message (gethash "rawOb" properties)))))))))

(defun get-metar (&optional (url *metar-url*))
  (when url
    (parse (octets-to-string (http-request url)))))
