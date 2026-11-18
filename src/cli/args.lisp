(in-package :structural-editing-mcp.cli)

(declaim (optimize (speed 2) (safety 3)))

(defun all-digits-p (str)
  "Return T if STR is a non-empty string of ASCII digits."
  (and (stringp str) (plusp (length str)) (every #'digit-char-p str)))

(defun split-string-by-char (str sep)
  "Split STR by character SEP, omitting empty segments."
  (loop with start = 0
        with len = (length str)
        for pos = (position sep str :start start)
        collect (subseq str start (or pos len)) into parts
        while pos
        do (setf start (1+ pos))
        finally (return (remove "" parts :test #'string=))))

(defun parse-cli-path (raw)
  "Parse a CLI path specification into an MCP path selector.
Supports:
  - NIL or empty string -> NIL
  - \"[]\" -> NIL (workspace root)
  - \"#n42\", \"#n42:a3f9\", \"n42\", or \"n42:a3f9\" -> handle string
  - \"0,0,2\" or \"[0, 0, 2]\" or \"0.0.2\" -> (0 0 2)
  - \"main.lisp/defun:main/2\" -> (\"main.lisp\" \"defun:main\" 2)"
  (when (null raw) (return-from parse-cli-path nil))
  (let
      ((s (trim-whitespace (format nil "~A" raw))))
    (when (or (zerop (length s)) (string= s "[]")) (return-from parse-cli-path nil))
    ;; Node / hashnode handle (#n42, #n42:a3f9, n42, or n42:a3f9)
    (let*
        ((colon-pos (position #\: s))
         (id-part
           (if colon-pos
             (subseq s 0 colon-pos)
             s))
         (hash-part (when colon-pos (subseq s (1+ colon-pos)))))
      (when
          (and
            (or (null hash-part)
                (and (plusp (length hash-part))
                     (every
                       (lambda (c)
                         (digit-char-p c 16))
                       hash-part)))
            (or
              (and (> (length id-part) 2)
                   (string-equal "#n" id-part :end2 2)
                   (all-digits-p (subseq id-part 2)))
              (and (> (length id-part) 1)
                   (char-equal (char id-part 0) #\n)
                   (all-digits-p (subseq id-part 1)))))
        (return-from parse-cli-path s)))
    ;; Strip surrounding brackets [ ... ] if present
    (when
        (and (>= (length s) 2)
             (char= (char s 0) #\[)
             (char= (char s (1- (length s))) #\]))
      (setf s (trim-whitespace (subseq s 1 (1- (length s)))))
      (when (zerop (length s)) (return-from parse-cli-path nil)))
    ;; Comma-separated (e.g. "0,0,2" or "0, 0, 2")
    (when (find #\, s)
      (return-from parse-cli-path
                   (mapcar
                     (lambda (seg)
                       (let ((trimmed (trim-whitespace seg)))
                         (if (all-digits-p trimmed)
                           (parse-integer trimmed)
                           trimmed)))
                     (split-string-by-char s #\,))))
    ;; Slash-separated symbolic path (e.g. "main.lisp/defun:main/2")
    (when (find #\/ s)
      (return-from parse-cli-path
                   (mapcar
                     (lambda (seg)
                       (if (all-digits-p seg)
                         (parse-integer seg)
                         seg))
                     (split-string-by-char s #\/))))
    ;; Dot-separated numeric path (e.g. "0.0.2") - only if all parts are digits
    (when (find #\. s)
      (let ((parts (split-string-by-char s #\.)))
        (when (every #'all-digits-p parts)
          (return-from parse-cli-path (mapcar #'parse-integer parts)))))
    ;; Single integer or single symbolic segment
    (if (all-digits-p s)
      (list (parse-integer s))
      (list s))))

(defun split-csv (str)
  "Split comma-separated string STR into a list of trimmed non-empty strings."
  (when (and str (plusp (length str)))
    (mapcar #'trim-whitespace (split-string-by-char str #\,))))

(defparameter *cli-boolean-flags*
  '("help" "version"
    "json"
    "commit"
    "in-place"
    "dry-run"
    "force"
    "quiet"
    "no-session"
    "no-daemon"
    "foreground"
    "stdin"
    "empty")
  "Flags that do not consume a following argument.")

(defparameter *cli-repeatable-flags*
  '("load" "rules" "files" "params")
  "Flags whose values accumulate into a list across repeated occurrences.")

(defparameter *cli-short-flag-aliases*
  '(("h" . "help") ("v" . "version")
    ("w" . "workspace")
    ("L" . "load")
    ("i" . "commit")
    ("q" . "quiet")
    ("p" . "path")
    ("d" . "depth")
    ("m" . "mode")
    ("n" . "limit")
    ("o" . "offset")
    ("a" . "action")
    ("f" . "file")
    ("F" . "force"))
  "Single-character short flag aliases.")

(defun normalize-flag-name (name)
  "Normalize a CLI flag name (underscores to hyphens, lowercase)."
  (string-downcase (substitute #\- #\_ name)))

(defun resolve-short-flag (short-name)
  "Resolve a single-character flag name via *CLI-SHORT-FLAG-ALIASES*."
  (or
    (cdr (assoc short-name *cli-short-flag-aliases* :test #'string=))
    (normalize-flag-name short-name)))

(defun set-flag-value (flags key val)
  "Store VAL for normalized flag KEY in FLAGS hash table (repeatable flags accumulate in reverse order)."
  (if
      (member key *cli-repeatable-flags* :test #'string=)
    (push val (gethash key flags))
    (setf (gethash key flags) val)))

(defun parse-cli-args (argv)
  "Parse ARGV (list of strings) into (values command positionals flags).
Supports --flag value, --flag=value, -f value, repeatable flags, and --."
  (let
      ((flags (make-hash-table :test #'equal))
       (positionals '())
       (stop-flags nil)
       (rest argv))
    (flet
        ((consume-flag (key raw-flag)
           (cond
             ((member key *cli-boolean-flags* :test #'string=) (setf (gethash key flags) t))
             (rest (set-flag-value flags key (pop rest)))
             (t (error "Flag ~A requires a value." raw-flag)))))
      (loop while rest
            for arg = (pop rest)
            do
            (cond
              (stop-flags (push arg positionals))
              ((string= arg "--") (setf stop-flags t))
              ((and (> (length arg) 2) (string-prefix-p "--" arg))
                (let*
                    ((body (subseq arg 2)) (eq-pos (position #\= body)))
                  (if eq-pos
                    (set-flag-value flags
                                    (normalize-flag-name (subseq body 0 eq-pos))
                                    (subseq body (1+ eq-pos)))
                    (let ((key (normalize-flag-name body)))
                      (consume-flag key (format nil "--~A" key))))))
              ((and (> (length arg) 1) (char= (char arg 0) #\-))
                (let*
                    ((short (subseq arg 1 2)) (key (resolve-short-flag short)))
                  (if (> (length arg) 2)
                    (set-flag-value flags key (subseq arg 2))
                    (consume-flag key (format nil "-~A" short)))))
              (t (push arg positionals)))))
    (dolist (rflag *cli-repeatable-flags*)
      (let ((vals (gethash rflag flags)))
        (when vals (setf (gethash rflag flags) (nreverse vals)))))
    (let*
        ((ordered (nreverse positionals))
         (command (first ordered))
         (rest-pos (rest ordered)))
      (values command rest-pos flags))))

(defun flag-get (flags key &optional default)
  "Retrieve a flag value from FLAGS."
  (multiple-value-bind (val present)
                       (gethash key flags)
    (if present
      val
      default)))

(defun flag-bool (flags &rest keys)
  "Return T if any of KEYS is truthy in FLAGS."
  (loop for k in keys for v = (gethash k flags) when v return t))

(defun flag-int (flags key &optional default)
  "Retrieve an integer flag value from FLAGS."
  (let ((val (gethash key flags)))
    (cond
      ((null val) default)
      ((integerp val) val)
      ((stringp val)
        (handler-case (parse-integer val)
          (error () (error "Flag --~A expects an integer, got ~S." key val))))
      (t default))))

(defun flag-string-list (flags key)
  "Retrieve a list of strings from repeatable or comma-separated flag KEY."
  (let ((raw (gethash key flags)))
    (when raw
      (loop for item in
            (if (listp raw)
              raw
              (list raw))
            append (split-csv item)))))

(defun read-stdin-string ()
  "Read all available text from *STANDARD-INPUT* as a string."
  (with-output-to-string (out)
    (loop for line = (read-line *standard-input* nil :eof)
          until (eq line :eof)
          do (write-line line out))))

(defun json-encode-to-string (obj)
  "Encode OBJ to a compact JSON string via YASON."
  (with-output-to-string (out) (yason:encode obj out)))