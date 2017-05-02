(in-package :robot)

(defcommand metar :user
    "Download and display latest METAR."
  (lambda (message)
    (let* ((last-called 0)
	   (time-since-last-call (abs (- (get-universal-time) last-called))))
      (if (> time-since-last-call 300)
	  (progn
	    (say message "Pretending to retrieve METAR...")
	    (setf last-called (get-universal-time)))
	  (say message (format nil "Please wait ~a second~:P before trying again." (- 300 time-since-last-call)))))))
