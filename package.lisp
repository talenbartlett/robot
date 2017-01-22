;;;; package.lisp

(defpackage #:robot
  (:use #:cl
	#:cl-irc
	#:alexandria
	#:bordeaux-threads
	#:ironclad)
  (:export :init :make-bot))

