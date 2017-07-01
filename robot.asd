;;;; robot.asd

(asdf:defsystem #:robot
  :serial t
  :description "IRC bot"
  :author "tjb"
  :license "MIT"
  :components ((:file "package")
	       (:file "config")
	       (:file "commands")
               (:file "robot")
	       (:file "utility")
	       (:file "speech")
	       (:file "hooks")
	       (:file "topic")
	       (:file "auth")
	       (:file "game")
	       (:file "math")
	       (:file "web")
	       (:file "memo"))
  :depends-on (:cl-irc
	       :alexandria
	       :bordeaux-threads
	       :cl-ppcre
	       :cl+ssl
	       :ironclad
	       :drakma
	       :yason))
