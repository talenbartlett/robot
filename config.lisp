(in-package :robot)

(defparameter *nick-prefixes* (loop for a from (char-code #\A) to (char-code #\z)
				 collect (code-char a)))

(defparameter *speaking* t)
(defparameter *training* nil)
(defparameter *metar-url* nil)
