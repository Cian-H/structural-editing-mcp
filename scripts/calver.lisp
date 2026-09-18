#!/usr/bin/env -S devenv shell -- sbcl --script

(require 'asdf)

(defun get-project-root ()
  (let ((this-file (or *load-truename* *load-pathname*)))
    (if this-file
      (uiop:pathname-parent-directory-pathname
        (uiop:pathname-directory-pathname this-file))
      (uiop:getcwd))))

(defun version-file-path ()
  (merge-pathnames "version.txt" (get-project-root)))

(defun read-current-version-file ()
  (let ((path (version-file-path)))
    (when (probe-file path)
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (uiop:read-file-line path)))))

(defun git-run (args &key (error-nil t))
  (handler-case
      (let ((output (uiop:run-program (cons "git" args)
                                      :directory (get-project-root)
                                      :output :string
                                      :error-output nil)))
        (string-trim '(#\Space #\Tab #\Newline #\Return) output))
    (error (c)
      (if error-nil
        nil
        (error c)))))

(defun git-available-p ()
  (not (null (git-run '("rev-parse" "--is-inside-work-tree") :error-nil t))))

(defun get-date-components (&key (use-git t))
  "Return (values year month day) from git or decoded time."
  (let ((git-date (when (and use-git (git-available-p))
                    (git-run '("log" "-1" "--format=%cd" "--date=format:%Y-%m-%d")))))
    (if (and git-date (>= (length git-date) 10))
      (let ((parts (uiop:split-string git-date :separator "-")))
        (values (parse-integer (first parts))
                (parse-integer (second parts))
                (parse-integer (third parts))))
      (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
        (declare (ignore sec min hr))
        (values yr mon day)))))

(defun count-commits-on-date (year month day)
  "Count git commits on HEAD for the given date."
  (if (git-available-p)
    (let* ((since-str (format nil "~D-~2,'0D-~2,'0D 00:00:00" year month day))
           (until-str (format nil "~D-~2,'0D-~2,'0D 23:59:59" year month day))
           (out (git-run (list "rev-list" "--count"
                               (format nil "--since=~A" since-str)
                               (format nil "--until=~A" until-str)
                               "HEAD"))))
      (if out (or (parse-integer out :junk-allowed t) 0) 0))
    0))

(defun compute-calver (&key next-p)
  "Compute the CalVer string for the repository."
  (multiple-value-bind (yr mon day) (get-date-components :use-git (not next-p))
    (when next-p
      (multiple-value-bind (sec min hr d m y) (get-decoded-time)
        (declare (ignore sec min hr))
        (setf yr y mon m day d)))
    (let* ((count (count-commits-on-date yr mon day))
           (effective-count (if next-p (1+ count) count)))
      (if (zerop effective-count)
        (format nil "~D.~D.~D" yr mon day)
        (format nil "~D.~D.~D.~D" yr mon day effective-count)))))

(defun update-version-file (&key next-p)
  (let* ((target-ver (compute-calver :next-p next-p))
         (current-ver (read-current-version-file))
         (path (version-file-path)))
    (if (equal target-ver current-ver)
      (progn
        (format t "version.txt is already up-to-date: ~A~%" target-ver)
        target-ver)
      (progn
        (with-open-file (out path :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "~A~%" target-ver))
        (format t "Updated version.txt: ~A -> ~A~%" (or current-ver "none") target-ver)
        target-ver))))

(defun create-git-tag (&optional ver)
  (unless (git-available-p)
    (format *error-output* "Error: Git repository not available.~%")
    (uiop:quit 1))
  (let* ((version (or ver (read-current-version-file) (compute-calver)))
         (tag-name (format nil "v~A" version))
         (tag-exists (git-run (list "rev-parse" "-q" "--verify" (format nil "refs/tags/~A" tag-name)))))
    (if tag-exists
      (format t "Git tag ~A already exists.~%" tag-name)
      (progn
        (git-run (list "tag" "-a" tag-name "-m" (format nil "Release ~A" version)) :error-nil nil)
        (format t "Created git tag ~A~%" tag-name)))))

(defun check-version ()
  (let* ((file-ver (read-current-version-file))
         (comp-ver (compute-calver)))
    (format t "File version:     ~A~%" (or file-ver "(missing)"))
    (format t "Computed CalVer:  ~A~%" comp-ver)
    (if (equal file-ver comp-ver)
      (progn
        (format t "Status: OK (synchronized)~%")
        (uiop:quit 0))
      (progn
        (format t "Status: DIVERGED (run --update to sync)~%")
        (uiop:quit 1)))))

(defun print-help ()
  (format t "Usage: ./scripts/calver.lisp [OPTION]~%~%")
  (format t "Automatic Calendar Versioning (CalVer) tool for structural-editing-mcp.~%~%")
  (format t "Options:~%")
  (format t "  (no args), --print   Print the current CalVer version~%")
  (format t "  --update             Update version.txt to match the current computed CalVer~%")
  (format t "  --next               Print the CalVer version for the next commit/push~%")
  (format t "  --tag                Tag HEAD with the current CalVer release tag (e.g. v2026.9.18.53)~%")
  (format t "  --check              Verify version.txt matches the computed CalVer~%")
  (format t "  -h, --help           Show this help message~%"))

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (cmd (first args)))
    (cond
      ((or (null cmd) (string= cmd "--print"))
        (format t "~A~%" (or (read-current-version-file) (compute-calver))))
      ((string= cmd "--next")
        (format t "~A~%" (compute-calver :next-p t)))
      ((string= cmd "--update")
        (update-version-file))
      ((string= cmd "--tag")
        (create-git-tag))
      ((string= cmd "--check")
        (check-version))
      ((or (string= cmd "-h") (string= cmd "--help"))
        (print-help))
      (t
        (format *error-output* "Unknown option: ~A~%" cmd)
        (print-help)
        (uiop:quit 1)))))

(main)
