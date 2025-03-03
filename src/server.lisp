(defpackage denote-wiki.server
  (:use :cl :clack :ningle)
  (:import-from :denote-wiki.note
                :*notes-dir*
                :find-note-by-title
                :pandoc
                :get-org-files
                :parse-note-filename)
  (:export :start-server
           :stop-server))

(in-package :denote-wiki.server)

(defvar *app* (make-instance 'ningle:app))

(defun serve-note (title)
  "Serve the Org note as HTML based on the title."
  (let ((note-path (find-note-by-title title)))
    (if note-path
        (pandoc note-path "org" "html")
        "<h1>Note not found</h1>")))

(setf (ningle:route *app* "/:title")
      #'(lambda (params)
          (serve-note (cdr (assoc :title params)))))

(setf (ningle:route *app* "/")
      (lambda (req)
        (let* ((org-files (get-org-files *notes-dir*))
               (recent-files (reverse
                              (subseq org-files (max 0 (- (length org-files) 10))))))
          ;; Debugging: print the number of org-files and their names
          (cl-who:with-html-output-to-string (s)
            (:html
             (:head (:title "Welcome to denote-wiki!"))
             (:body
              (:h1 "Welcome to denote-wiki!")
              (:h2 "Recent Notes")
              (:ul
               (dolist (file recent-files)
                 (let ((parsed (parse-note-filename file)))
                   (when parsed ;; Ensure parsing was successful
                     (destructuring-bind (date time title keywords) parsed
                       (cl-who:htm
                        (:li
                         (cl-who:str date) " " (cl-who:str time)
                         " "
                         (cl-who:htm (:strong
                                      (cl-who:str
                                       (substitute #\Space #\-
                                                   (string-capitalize title)))))
                         " - Keywords: "
                         (cl-who:htm (:em (cl-who:str (format nil "~{~a~^, ~}" keywords)))))))))))
              (:h2 "Search By Keyword")
              ))))))

(defun start-server ()
  (clack:clackup *app*
                 :server :woo))
