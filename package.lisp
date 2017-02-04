;;;; package.lisp

(defpackage #:robot
  (:use #:cl
	#:cl-irc
	#:alexandria
	#:bordeaux-threads
	#:cl-ppcre)
  (:import-from :ironclad :pbkdf2-hash-password-to-combined-string
		:ascii-string-to-byte-array
		:pbkdf2-check-password)
  (:export :init :make-bot))

