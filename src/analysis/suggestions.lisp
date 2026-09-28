(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

(defstruct refactoring-suggestion
  category
  priority
  path
  description
  recommended-tool
  action-plan)

(defun priority-rank (p)
  "Return numeric rank for PRIORITY keyword (:high = 3, :medium = 2, :low = 1)."
  (case p
    (:high 3)
    (:medium 2)
    (:low 1)
    (t 0)))

(defun parse-priority-keyword (val)
  "Parse a priority value into :high, :medium, or :low keyword."
  (cond
    ((null val) :low)
    ((symbolp val) (intern (string-upcase (symbol-name val)) :keyword))
    ((stringp val) (intern (string-upcase (string-left-trim ":" val)) :keyword))
    (t :low)))

(defun parse-category-keyword (val)
  "Parse a category string or keyword into :lint, :complexity, :duplicate, or :binding."
  (let ((s (if (symbolp val) (symbol-name val) (string val))))
    (intern (string-upcase (string-left-trim ":" s)) :keyword)))

(defun category-active-p (cat-keywords primary-keyword &optional alias)
  "Check if PRIMARY-KEYWORD or ALIAS is selected in CAT-KEYWORDS list (or if list is empty)."
  (or (null cat-keywords)
      (member primary-keyword cat-keywords)
      (and alias (member alias cat-keywords))))

(defun priority-meets-threshold-p (priority min-rank)
  (>= (priority-rank priority) min-rank))

(defun collect-lint-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from the anti-pattern linter."
  (let ((suggestions '())
        (lint-findings (lint-ast tree :path path :dialect dialect)))
    (dolist (f lint-findings)
      (let* ((rule (lint-finding-rule f))
             (p (case rule
                  ((:single-clause-cond :if-boolean-redundant
                    :ignored-destructive-return :unhygienic-macro-binding
                    :clojure-tail-recur :mutate-literal-constant)
                   :high)
                  ((:if-progn-to-when :if-nil-to-when
                    :if-not-to-unless :invert-if-not
                    :equal-nil-to-null :special-var-earmuffs
                    :clojure-swap-side-effects :dead-cond-clauses
                    :inappropriate-equality :clojure-vector-contains)
                   :medium)
                  (t :low)))
             (tool (case rule
                     ((:redundant-progn :dead-cond-clauses) "ast_remove")
                     ((:special-var-earmuffs :unhygienic-macro-binding) "ast_rename")
                     (t "ast_modify")))
             (plan (if (lint-finding-suggested-fix f)
                     (format nil "Apply structural replacement: ~A" (lint-finding-suggested-fix f))
                     "Refactor expression using recommended pattern.")))
        (when (priority-meets-threshold-p p min-rank)
          (push (make-refactoring-suggestion :category :lint
                                             :priority p
                                             :path (lint-finding-path f)
                                             :description (lint-finding-message f)
                                             :recommended-tool tool
                                             :action-plan plan)
                suggestions))))
    (nreverse suggestions)))

(defun complexity-metric->suggestion (m min-rank)
  "Convert a complexity metric M to a refactoring suggestion if it meets MIN-RANK."
  (let* ((cc (complexity-metrics-cyclomatic-complexity m))
         (depth (complexity-metrics-max-nesting-depth m))
         (p (cond
              ((or (>= cc 15) (>= depth 8)) :high)
              ((or (>= cc 10) (>= depth 6)) :medium)
              (t :low))))
    (when (priority-meets-threshold-p p min-rank)
      (let ((tool (if (>= cc 10)
                    "ast_extract_function"
                    "ast_extract_variable"))
            (plan (format nil "Decompose ~A ~A: ~{~A~^ ~}"
                          (complexity-metrics-kind m)
                          (complexity-metrics-name m)
                          (complexity-metrics-recommendations m))))
        (make-refactoring-suggestion :category :complexity
                                     :priority p
                                     :path (complexity-metrics-path m)
                                     :description (format nil "Form has cyclomatic complexity ~A and nesting depth ~A." cc depth)
                                     :recommended-tool tool
                                     :action-plan plan)))))

(defun collect-complexity-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from complexity metrics analysis."
  (let ((complex-forms (analyze-complexity tree :path path :dialect dialect :min-complexity 8 :min-depth 4))
        (suggestions '()))
    (dolist (m complex-forms (nreverse suggestions))
      (let ((s (complexity-metric->suggestion m min-rank)))
        (when s (push s suggestions))))))

(defun duplicate-group->suggestion (g min-rank)
  "Convert a duplicate subtree group G to a refactoring suggestion if it meets MIN-RANK."
  (let* ((savings (* (duplicate-group-node-count g) (1- (duplicate-group-occurrence-count g))))
         (p (cond
              ((>= savings 20) :high)
              ((>= savings 8) :medium)
              (t :low))))
    (when (priority-meets-threshold-p p min-rank)
      (let ((tool (if (search "ast_extract_variable" (duplicate-group-recommendation g))
                    "ast_extract_variable"
                    "ast_extract_function"))
            (plan (format nil "~A (Saves ~A AST nodes across ~A occurrences)"
                          (duplicate-group-recommendation g)
                          savings
                          (duplicate-group-occurrence-count g))))
        (make-refactoring-suggestion :category :duplicate
                                     :priority p
                                     :path (first (duplicate-group-paths g))
                                     :description (format nil "Code clone repeated ~A times: ~A"
                                                          (duplicate-group-occurrence-count g)
                                                          (duplicate-group-code-snippet g))
                                     :recommended-tool tool
                                     :action-plan plan)))))

(defun collect-duplicate-suggestions (tree path min-rank)
  "Collect refactoring suggestions generated from AST duplicate subtree detection."
  (let ((duplicate-groups (find-duplicate-subtrees tree :path path :min-nodes 5 :min-depth 2))
        (suggestions '()))
    (dolist (g duplicate-groups (nreverse suggestions))
      (let ((s (duplicate-group->suggestion g min-rank)))
        (when s (push s suggestions))))))

(defun binding-finding->suggestion (f min-rank)
  "Convert a binding finding F to a refactoring suggestion if it meets MIN-RANK."
  (let* ((is-shadowed (eq (binding-finding-kind f) :shadowed-variable))
         (p (if is-shadowed :high :medium)))
    (when (priority-meets-threshold-p p min-rank)
      (let ((tool (if is-shadowed "ast_rename" "ast_remove"))
            (plan (binding-finding-recommendation f)))
        (make-refactoring-suggestion :category :binding
                                     :priority p
                                     :path (binding-finding-path f)
                                     :description (binding-finding-message f)
                                     :recommended-tool tool
                                     :action-plan plan)))))

(defun collect-binding-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from lexical scope & binding analysis."
  (let ((binding-findings (analyze-bindings tree :path path :dialect dialect))
        (suggestions '()))
    (dolist (f binding-findings (nreverse suggestions))
      (let ((s (binding-finding->suggestion f min-rank)))
        (when s (push s suggestions))))))

(defun sort-refactoring-suggestions (suggestions)
  "Sort SUGGESTIONS descending by priority rank, breaking ties alphabetically by category."
  (sort suggestions
        (lambda (a b)
          (let ((r-a (priority-rank (refactoring-suggestion-priority a)))
                (r-b (priority-rank (refactoring-suggestion-priority b))))
            (if (= r-a r-b)
              (string< (string (refactoring-suggestion-category a))
                       (string (refactoring-suggestion-category b)))
              (> r-a r-b))))))

(defun suggest-refactorings (tree &key path (min-priority :low) categories (dialect *current-dialect*))
  "Aggregate findings from linting, complexity metrics, clone detection, and binding analysis.
Filters by MIN-PRIORITY (:high, :medium, :low) and CATEGORIES (list of category keywords or strings).
Returns a list of REFACTORING-SUGGESTION instances sorted by priority."
  (let* ((min-rank (priority-rank (parse-priority-keyword min-priority)))
         (cat-keywords (when categories (mapcar #'parse-category-keyword categories)))
         (suggestions '()))
    (when (category-active-p cat-keywords :lint)
      (setf suggestions (nconc (collect-lint-suggestions tree path dialect min-rank) suggestions)))
    (when (category-active-p cat-keywords :complexity)
      (setf suggestions (nconc (collect-complexity-suggestions tree path dialect min-rank) suggestions)))
    (when (category-active-p cat-keywords :duplicate :duplicates)
      (setf suggestions (nconc (collect-duplicate-suggestions tree path min-rank) suggestions)))
    (when (category-active-p cat-keywords :binding :bindings)
      (setf suggestions (nconc (collect-binding-suggestions tree path dialect min-rank) suggestions)))
    (sort-refactoring-suggestions suggestions)))

(defun format-refactoring-suggestions (suggestions)
  "Format a list of REFACTORING-SUGGESTION instances into an executive refactoring report."
  (if (null suggestions)
    "No refactoring opportunities detected matching criteria."
    (let* ((total (length suggestions))
           (high-count (count :high suggestions :key #'refactoring-suggestion-priority))
           (med-count (count :medium suggestions :key #'refactoring-suggestion-priority))
           (low-count (count :low suggestions :key #'refactoring-suggestion-priority))
           (lint-count (count :lint suggestions :key #'refactoring-suggestion-category))
           (comp-count (count :complexity suggestions :key #'refactoring-suggestion-category))
           (dup-count (count :duplicate suggestions :key #'refactoring-suggestion-category))
           (bind-count (count :binding suggestions :key #'refactoring-suggestion-category)))
      (with-output-to-string (s)
        (format s "========================================================~%")
        (format s "    Structural Refactoring Plan (~A Opportunit~:@P)     ~%" total)
        (format s "========================================================~%")
        (format s "Summary by Priority:~%")
        (format s "  - HIGH:   ~A~%" high-count)
        (format s "  - MEDIUM: ~A~%" med-count)
        (format s "  - LOW:    ~A~%~%" low-count)
        (format s "Summary by Category:~%")
        (format s "  - Anti-pattern Linting: ~A~%" lint-count)
        (format s "  - Structural Complexity: ~A~%" comp-count)
        (format s "  - Duplicate Code Clones: ~A~%" dup-count)
        (format s "  - Variable Bindings:     ~A~%~%" bind-count)
        (format s "--------------------------------------------------------~%")
        (format s "Prioritized Action Items:~%~%")
        (loop for item in suggestions for idx from 1 for
              p = (refactoring-suggestion-priority item) for
              cat = (refactoring-suggestion-category item) for
              path = (refactoring-suggestion-path item) for
              desc = (refactoring-suggestion-description item) for
              tool = (refactoring-suggestion-recommended-tool item) for
              plan = (refactoring-suggestion-action-plan item) do
              (format s
                      "~A. [~A | ~A] [~{~A~^, ~}]~%   Problem: ~A~%   Tool:    ~A~%   Action:  ~A~%~%"
                      idx (string p) (string cat) path desc tool plan))))))
