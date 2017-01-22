(in-package :robot)

(defun make-nick ()
  (format nil "~A" (alexandria:random-elt *nick-prefixes* 
					    :end (length *nick-prefixes*))))

(defun save-table (path table)
  (with-open-file (s (merge-pathnames
		      (make-pathname :directory '(:relative ".robot") :name path)
		      (user-homedir-pathname))
		   :direction :output
		   :if-exists :supersede)
    (write (alexandria:hash-table-alist table) :stream s)))

(defun read-table (path)
  (with-open-file (s (merge-pathnames
		      (make-pathname :directory '(:relative ".robot") :name path)
		      (user-homedir-pathname))
		     :direction :input
		     :if-does-not-exist nil)
    (when s
      (alexandria:alist-hash-table (read s) :test #'equal))))

(defun read-hooks-file ()
  (with-open-file (stream "hooks" :direction :input
			          :if-does-not-exist nil)
    (when stream
      (loop for hook = (read stream nil)
	   while hook do (push hook *hooks*)))))

(defun save-hooks-file ()
  (with-open-file (stream "hooks" :direction :output
			          :if-exists :supersede)
    (when stream
      (print *hooks* stream))))

(defun update-hooks (connection)
  (loop for (message fn) in *hooks*
     do (remove-hooks connection message)
        (add-hook connection message (coerce fn 'function))))

(defun destination (message)
  (first (arguments message)))

(defun message-string (message)
  (second (arguments message)))

(defun arguments-to-string (message)
  (format nil "~a" (arguments message)))

(defun message-to-string (message)
  (format nil "~{~S~}" (message-list message)))

(defun split-whitespace (string)
  (cl-ppcre:split "\\s+" string))

(defun file-to-train (filepath)
  (coerce (with-open-file (stream filepath :if-does-not-exist nil) 
	    (loop for char = (read-char stream nil)
	       until (eq char nil)
	       unless (eq char #\Newline) collect char)) 'string))
