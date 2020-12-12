(defun hello-world ()
  (format t "~a~%" "Hello, world.")
  (format t "~{~a~%~}" sb-ext:*posix-argv*))

(hello-world)
