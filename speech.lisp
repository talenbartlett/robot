(in-package :robot)

(defparameter *dictionary* (make-hash-table :test #'equal))
(defparameter *banned-words* nil)

(defun save-dictionary ()
  (save-table "dictionary" *dictionary*))
  
(defun read-dictionary ()
  (let ((table (read-table "dictionary")))
    (when table
      (setf *dictionary* table))))
  
(defun read-banned-words ()
  (let ((table (read-table "banned-words")))
    (when table
      (setf *banned-words* table))))

(defcommand speak :user
    "Allow the bot to speak. !speak"
  (lambda (message)
    (progn
      (start-speaking)
      (say message "Speaking now."))))

(defcommand quiet :user
    "Quiet the bot. !quiet"
  (lambda (message)
    (progn
      (stop-speaking)
      (say message "Stopped speaking."))))

(defcommand train :admin
    "Add words to the dictionary."
  (lambda (message)
    (with-slots (arguments) message
      (let ((command-args (command-arguments arguments)))
	(progn
	  (loop for x from 1 to 3 do (train command-args :order x))
	  (save-dictionary)
	  (say message (format nil "Added ~a to the dictionary." command-args)))))))

(defcommand reload-dict :admin
    "Reload dictionary while online."
  (lambda (message)
    (progn
      (read-dictionary)
      (say message "Dictionary reloaded."))))

(defcommand query-dict :user
    "Search the bot's dictionary."
  (lambda (message)
    (with-slots (arguments) message
      (let* ((command-args (command-arguments arguments))
	     (query-result (gethash command-args *dictionary*)))
	(say message (format nil "Result for ~a: ~a" command-args query-result))))))

(defun speak (message)
  (when *speaking*
   (with-slots (connection arguments) message
     (let ((split-msg (split-message message)))
       (when split-msg
	 (cond ((zerop (random 2)) (say message (response-sentence (get-fragment split-msg))))))))))

(defun stop-speaking ()
  (setf *speaking* nil))

(defun start-speaking ()
  (setf *speaking* t))

;;;;inspiration: http://stackoverflow.com/questions/5306729/how-do-markov-chain-chatbots-work
(defun train (word-list &key (order 1))
  (if (> (length word-list) order) 
      (loop for n from 0
	 for key = (subseq word-list 0 order) then (subseq word-list n (+ order n))
	 for val = (elt word-list order) then (elt word-list (+ order n))
	 unless (loop for word in key 
		   for value = val 
		   when (or (member word *banned-words* :test #'string-equal)
			    (member value *banned-words* :test #'string-equal))
		   return t)
	 do (add-word key val) until (= n (- (length word-list) order 1)))))

(defun split-message (message)
  (split-whitespace (message-string message)))

(defun get-fragment (string-list)
  (nthcdr (random (length string-list)) string-list))

(defun add-word (key value &key (table *dictionary*))
  (pushnew value (gethash key table) :test #'string-equal))

(defun match-word (word table)
  (loop for key being the hash-keys in table 
     when (member word key :test #'string-equal) collect key))

(defun match-fragment (fragment table)
  (loop for key being the hash-keys in table
     when (subsetp fragment key :test #'string-equal) collect key))

(defun random-match (matches)
  (if matches 
      (nth (random (length matches)) matches)))

(defun random-value (key table)
  (let ((hash (gethash key table)))
    (if (and key
	     hash) 
	(list (nth (random (length hash)) hash)))))

(defun response-sentence (fragment &key (table *dictionary*))
  (loop for matches = (match-fragment fragment table) then (match-word (car picked-value) table)
     for picked-key = (random-match matches)
     for picked-value = (random-value picked-key table)
     for sentence = (append picked-key picked-value) then (append sentence picked-value)
     when (or (scan "[\!\.\?]$" (car picked-value))
	      (null picked-value)) 
     do (return (format nil "~{~a~^ ~}" sentence))))
