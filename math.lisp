(in-package :robot)

(defcommand base :user
    "Convert a decimal number to another base. !base <number> <new base>"
  (lambda (message)
    (with-slots (arguments) message
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
