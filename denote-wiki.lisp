(ql:quickload '(:ningle :clack :alexandria :cl-who :cl-ppcre))

;; ----------
;; Note logic
;; ----------

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
  "Parse denote filename into list (YYYY-MM-DD HH:MM:SS title '(keywords))"
  (cl-ppcre:register-groups-bind (date time title keywords)
      ("(\\d{8})T(\\d{6})--(.*?)__([^.]*)\\.org"
       (file-namestring filepath))
    (let* ((formatted-date (concatenate 'string
                                        (subseq date 0 4) "-"  ; YYYY
                                        (subseq date 4 6) "-"  ; MM
                                        (subseq date 6 8)))    ; DD
           (formatted-time (concatenate 'string
                                        (subseq time 0 2) ":"  ; HH
                                        (subseq time 2 4) ":"  ; MM
                                        (subseq time 4 6)))    ; SS
           (keywords-list (cl-ppcre:split "_" keywords)))
      (list formatted-date formatted-time title keywords-list))))

(defun find-note-by-title (title)
  "Search for an Org file matching TITLE in *notes-dir* and return its full path."
  (loop for file in (get-org-files *notes-dir*)
        for parsed = (parse-note-filename file)
        when (string= (third parsed) title)
          return (namestring file)))

;; ----------
;; Conversion
;; ----------

(defun pandoc (input-path from to)
  (let* ((command-str
           (format nil "pandoc ~a -f ~a -t ~a" input-path from to)))
    (uiop:run-program command-str :output :string)))

(defun serve-note (title)
  "Serve the Org note as HTML based on the title."
  (let ((note-path (find-note-by-title title)))
    (if note-path
        (pandoc note-path "org" "html")
        "<h1>Note not found</h1>")))

;; ----------
;; Hosting html
;; ----------
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/:title")
      #'(lambda (params)
          (serve-note (cdr (assoc :title params)))))

(setf (ningle:route *app* "/")
      (lambda (req)
        (let ((org-files (get-org-files *notes-dir*)))  ;; Ensure *notes-dir* is correctly set
          (cl-who:with-html-output-to-string (s)
            (:html
             (:head (:title "Welcome to denote-wiki!"))
             (:body
              (:h1 "Welcome to denote-wiki!")
              (:h2 "Recent notes")
              (:ul
               (dolist (file org-files)
                 (let ((parsed (parse-note-filename file)))
                   (when parsed  ;; Ensure parsing was successful
                     (destructuring-bind (date time title keywords) parsed
                       (cl-who:htm
                        (:li
                         (cl-who:str date) " " (cl-who:str time)
                         " "
                         (cl-who:htm (:strong (cl-who:str (substitute #\Space #\- (string-capitalize title)))))   ;; Display title in bold
                         " - Keywords: "
                         (cl-who:htm (:em (cl-who:str (format nil "~{~a~^, ~}" keywords)))))))))))))))))


(clack:clackup *app*)

(defun parse-note-filename (filepath)
  "Parse denote filename into list (YYYY-MM-DD HH:MM:SS title '(keywords))"
  (cl-ppcre:register-groups-bind (date time title keywords)
      ("(\\d{8})T(\\d{6})--(.*?)__([^.]*)\\.org"
       (file-namestring filepath))
    (let* ((formatted-date (concatenate 'string
                                        (subseq date 0 4) "-"  ; YYYY
                                        (subseq date 4 6) "-"  ; MM
                                        (subseq date 6 8)))    ; DD
           (formatted-time (concatenate 'string
                                        (subseq time 0 2) ":"  ; HH
                                        (subseq time 2 4) ":"  ; MM
                                        (subseq time 4 6)))    ; SS
           (formatted-title (substitute #\Space #\- title))   ;; Replace '-' with ' '
           (keywords-list (cl-ppcre:split "_" keywords)))
      (list formatted-date formatted-time formatted-title keywords-list))))
