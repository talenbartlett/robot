;;;; robot.asd

(asdf:defsystem #:robot
  :serial t
  :description "IRC bot"
  :author "tjb"
  :license "MIT"
  :components ((:file "package")
               (:file "robot")
	       (:file "utility")
	       (:file "speech")
	       (:file "commands")
	       (:file "hooks")
	       (:file "topic")
	       (:file "auth"))
  :depends-on (:cl-irc :alexandria :bordeaux-threads :cl-ppcre :cl+ssl :ironclad))
