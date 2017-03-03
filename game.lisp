(in-package :robot)

(defcommand roll :user
    "Roll dice. !roll <#dice> <#sides>"
  (lambda (message)
    (with-slots (arguments) message
      (let ((command-args (command-arguments arguments)))
	(if (= (length command-args) 2)
	    (let* ((number-of-dice (parse-integer (first command-args) :junk-allowed t))
		   (sides (parse-integer (second command-args) :junk-allowed t))
		   (rolls (when
			      (and (numberp number-of-dice) (> number-of-dice 0) (<= number-of-dice 10000)
				   (numberp sides) (> sides 0) (<= sides 10000))
			    (loop repeat number-of-dice collect (1+ (random sides))))))
	      (say message (format nil "~a | Sum = ~a" rolls (reduce '+ rolls))))
	    (say message "Nothing to roll."))))))

