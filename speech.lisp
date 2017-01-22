(in-package :robot)

(defparameter *dictionary* (make-hash-table :test #'equal))
(defparameter *banned-words* nil)

(defun save-dictionary (table)
  (save-table "dictionary" table))
  
(defun read-dictionary (table)
  (read-table "dictionary" table))
  
(defun read-banned-words (ban-list)
  (read-table "banned-words" ban-list))
  
;;;;inspiration: http://stackoverflow.com/questions/5306729/how-do-markov-chain-chatbots-work
(defun train (word-list &key (order 3))
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
  (cl-ppcre:split "\\s+" (message-string message)))

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
  (if (null matches) 
      nil
      (nth (random (length matches)) matches)))

(defun random-value (key table)
  (let ((hash (gethash key table)))
    (if (null (and key
		  hash)) 
	nil
	(list (nth (random (length hash)) hash)))))

(defun response-sentence (fragment &key (table *dictionary*))
  (loop for matches = (match-fragment fragment table) then (match-word (car picked-value) table)
     for picked-key = (random-match matches)
     for picked-value = (random-value picked-key table)
     for sentence = (append picked-key picked-value) then (append sentence picked-value)
     when (or (cl-ppcre:scan "[\!\.\?]$" (car picked-value))
	      (null picked-value)) 
     do (return (format nil "~{~a~^ ~}" sentence))))
