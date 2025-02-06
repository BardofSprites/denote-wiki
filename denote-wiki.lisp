(ql:quickload "cl-ppcre")
(ql:quickload "hunchentoot")
(ql:quickload "uiop")

;;;; Note logic
(defparameter *notes-dir* "~/Notes/denote/")

(defun get-org-files (directory)
  (remove-if-not (lambda (it)
                   (search ".org" (namestring it)))
                 (uiop:directory-files directory)))

(defun extract-keywords (filename)
  "Extract and split the keywords (tags) from a filename."
  (let ((start (search "__" filename)))
    (when start
      (let ((tag-string (subseq filename
                                (+ start 2)
                                (position #\. filename :from-end t))))
        (mapcar #'intern (ppcre:split #\_ tag-string))))))

(defun find-keywords (directory keywords)
  "@params:
   directory - notes directory
   keywords - list of keywords to search
   @returns: list of filepaths that containe keywords in directory"
  (let ((keyword-symbols (mapcar #'intern keywords))) ; Convert keywords to symbols
    (remove-if-not (lambda (file)
                     (let ((tags (extract-keywords (namestring file))))
                       (and tags (every (lambda (kw)
                                          (member kw tags))
                                        keyword-symbols))))
                   (get-org-files directory))))


(defun unique-keywords (directory)
  "@params: directory - notes directory
   @returns: list of unique keywords"
  (let* ((file-names (mapcar #'namestring (get-org-files directory)))
         (keyword-symbols (mapcan #'extract-keywords file-names))
         (keywords (mapcar (lambda (sym) (symbol-name sym)) keyword-symbols)))
    (remove-duplicates keywords)
    ))

(defun count-unique-keywords (directory)
  "@params: directory - notes directory
   @returns: number of unique keywords"
  (let* ((file-names (mapcar #'namestring (get-org-files directory)))
         (keyword-symbols (mapcan #'extract-keywords file-names))
         (keywords (mapcar (lambda (sym) (symbol-name sym)) keyword-symbols)))
    (length (remove-duplicates keywords))
    ))

(defun parse-note-filename (filepath)
  "Parse denote filename into list (YYYYMMDD HHMMSS title '(keywords))"
  (cl-ppcre:register-groups-bind (date time title keywords)
      ("(\\d{8})T(\\d{6})--(.*?)__([^.]*)\\.org"
       (file-namestring filepath))
    (list date time title (cl-ppcre:split "_" keywords))))

(defun format-date (date-str)
  (let* ((date (parse-integer date-str))
         (year (truncate date 10000))
         (month (truncate (mod date 10000) 100))
         (day (mod date 100)))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun format-keywords (keywords-list)
  (format nil "[~{~a~^, ~}]" keywords-list))

(defun format-time (time-str)
  (let* ((time (parse-integer time-str))
         (hour (truncate time 10000))
         (minute (truncate (mod time 10000) 100))
         (second (mod time 100)))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

;;;; Server

(defvar *server* nil)

(defun start-server ()
  "Start the local HTTP server."
  (setq *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8079))))

(defun stop-server ()
  "Stop the local HTTP server."
  (when *server*
    (hunchentoot:stop *server*)))

(defun render-file-list (files)
  "Generate HTML links for a list of files, stripping directory paths."
  (format nil "<ul>~{<li><a href='/note/~a'>~a</a></li>~}</ul>"
          (mapcar #'pathname-name files) (mapcar #'pathname-name files)))

(defun browse-notes-page ()
  "Return an HTML page listing all notes."
  (let ((files (get-org-files *notes-dir*)))
    (with-output-to-string (s)
      (format s "<html><head><title>Notes</title></head>
                 <body><h1>Notes</h1>~a</body></html>"
              (render-file-list files)))))

(defun note-page (filename)
  "Return an HTML page displaying a specific note."
  (let ((file-path (merge-pathnames filename *notes-dir*)))
    (if (probe-file file-path)
        (with-open-file (stream file-path)
          (when stream
            (let ((content (make-string (file-length stream))))
              (read-sequence content stream)
              (format nil "<html><body><h1>Note: ~a</h1><pre>~a</pre></body></html>"
                      filename content))))
        "<html><body><h1>404 - Note not found</h1></body></html>")))

(hunchentoot:define-easy-handler (browse-notes :uri "/") ()
  (browse-notes-page))

(hunchentoot:define-easy-handler (view-note :uri "/note/:filename") (filename)
  (note-page filename))

;;;; Main
(defun -main ()
  (start-server))
