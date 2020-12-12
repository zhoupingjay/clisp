;; Exercises for learning LISP...

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped (y/n): ")))

(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? (y/n): ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector)
  (remove-if-not selector *db*))

(defun update (selector &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda(cd)
             (when (funcall selector cd)
               (if title (setf (getf cd :title) title))
               (if artist (setf (getf cd :artist) artist))
               (if rating (setf (getf cd :rating) rating))
               (if ripped (setf (getf cd :ripped) ripped)))
             cd) *db*)))
