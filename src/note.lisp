(defpackage :denote-wiki.note
  (:use :cl :cl-ppcre :uiop)
  (:export
   :*notes-dir*
   :get-org-files
   :find-note-by-title
   :parse-note-filename
   :unique-keywords
   :count-unique-keywords
   :find-keywords
   :pandoc))

(in-package :denote-wiki.note)

(defparameter *notes-dir* "~/Notes/denote/" "Directory containing note files")

(defun get-org-files (directory)
  (remove-if-not (lambda (it)
                   (and (search ".org" (file-namestring it))
                        (digit-char-p (char (file-namestring it) 0))))
                 (uiop:directory-files directory)))

(defun extract-keywords (filename)
  "Extract and split the keywords (tags) from a filename."
  (let ((start (search "__" filename)))
    (when start
      (let ((tag-string (subseq filename
                                (+ start 2)
                                (position #\. filename :from-end t))))
        (mapcar #'intern (ppcre:split #\_ tag-string))))))

(defun find-keywords (keywords)
  "@params:
   keywords - list of keywords to search
   @returns: list of filepaths that containe keywords in directory"
  (let ((keyword-symbols (mapcar #'intern keywords))) ; Convert keywords to symbols
    (remove-if-not (lambda (file)
                     (let ((tags (extract-keywords (namestring file))))
                       (and tags (every (lambda (kw)
                                          (member kw tags))
                                        keyword-symbols))))
                   (get-org-files *notes-dir*))))


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

(defun pandoc (input-path from to)
  "Convert the org note to html using pandoc."
  (let* ((command-str
           (format nil "pandoc --mathjax ~a -f ~a -t ~a" input-path from to)))
    (uiop:run-program command-str :output :string)))
