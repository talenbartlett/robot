(in-package :robot)

(defparameter *current-votes* (make-hash-table :test 'eql))
(defparameter *vote-tally* (make-hash-table :test 'equalp))

(defcommand vote :user
    "Start a vote. !vote kick <user> [reason]"
  (lambda (message)
    (with-slots (connection arguments source) message
      (let ((command-args (split "\\s+" (second arguments) :limit 3))
            (channel (find-channel connection (first arguments))))
        (if (and channel (not (currently-voting-p channel)))
            (progn
              (setf (gethash channel *vote-tally*) (make-list 2))
              (switch ((second command-args) :test 'equalp)
                ("kick" (let ((kick-args (split "\\s+" (third command-args) :limit 2)))
                          (if (string-equal (nickname (user connection)) (first kick-args))
                              (privmsg connection channel "Nice try!")
                              (progn (setf (gethash channel *current-votes*) (list :kick kick-args))
                                     (make-thread (lambda () (vote-handler connection channel)))))))
                ("rule" (setf (gethash channel *current-votes*) (list :rule (third command-args)))
                        (make-thread (lambda () (vote-handler connection channel))))
                (t (say message "This is not a recognized poll option."))))
            (switch ((second command-args) :test 'equalp)
              ("yes" (pushnew source (first (gethash channel *vote-tally*)) :test 'equal))
              ("no"  (pushnew source (second (gethash channel *vote-tally*)) :test 'equal))))))))

(defun vote-handler (connection channel)
  (loop for i from 3 downto 1 do
       (privmsg connection channel (format nil "~a seconds remaining to vote. Enter !vote yes or !vote no." (* i 10)))
       (sleep 10))
  (let* ((current-vote (gethash channel *current-votes*))
         (action (first current-vote))
         (tally (gethash channel *vote-tally*))
         (yes-votes (length (first tally)))
         (no-votes (length (second tally)))
         (total-votes (+ yes-votes no-votes)))
    (if (> yes-votes no-votes) ;not really representative of the total population...
        (progn (privmsg connection channel
                        (format nil "Vote succeeded. Action: ~a with ~a YES votes, ~a NO votes." action yes-votes no-votes))
               (case action (:kick (kick connection (name channel) (first (second current-vote)) (second (second current-vote))))))
        (privmsg connection channel (format nil "Vote failed."))))
  (remhash channel *current-votes*)
  (remhash channel *vote-tally*))

(defun currently-voting-p (channel)
  (not (null (gethash channel *current-votes*))))
