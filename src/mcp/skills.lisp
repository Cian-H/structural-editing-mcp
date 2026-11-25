(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; Pure Common Lisp SHA-256 (RFC 6234) for SEP-2640 Skill Manifest Digests

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491
    #xb5c0fbcf
    #xe9b5dba5
    #x3956c25b
    #x59f111f1
    #x923f82a4
    #xab1c5ed5
    #xd807aa98
    #x12835b01
    #x243185be
    #x550c7dc3
    #x72be5d74
    #x80deb1fe
    #x9bdc06a7
    #xc19bf174
    #xe49b69c1
    #xefbe4786
    #x0fc19dc6
    #x240ca1cc
    #x2de92c6f
    #x4a7484aa
    #x5cb0a9dc
    #x76f988da
    #x983e5152
    #xa831c66d
    #xb00327c8
    #xbf597fc7
    #xc6e00bf3
    #xd5a79147
    #x06ca6351
    #x14292967
    #x27b70a85
    #x2e1b2138
    #x4d2c6dfc
    #x53380d13
    #x650a7354
    #x766a0abb
    #x81c2c92e
    #x92722c85
    #xa2bfe8a1
    #xa81a664b
    #xc24b8b70
    #xc76c51a3
    #xd192e819
    #xd6990624
    #xf40e3585
    #x106aa070
    #x19a4c116
    #x1e376c08
    #x2748774c
    #x34b0bcb5
    #x391c0cb3
    #x4ed8aa4a
    #x5b9cca4f
    #x682e6ff3
    #x748f82ee
    #x78a5636f
    #x84c87814
    #x8cc70208
    #x90befffa
    #xa4506ceb
    #xbef9a3f7
    #xc67178f2))

(defun u32+ (&rest args) (logand #xffffffff (reduce #'+ args :initial-value 0)))

(defun rotr32 (x n) (logand #xffffffff (logior (ash x (- n)) (ash x (- 32 n)))))

(defun sha256-hex (text)
  "Compute lowercase hex SHA-256 digest of UTF-8 string TEXT."
  (let*
      ((bytes (sb-ext:string-to-octets (or text "") :external-format :utf-8))
       (bit-len (* (length bytes) 8))
       (pad-len (mod (- 56 (mod (1+ (length bytes)) 64)) 64))
       (padded
         (make-array (+ (length bytes) 1 pad-len 8)
                     :element-type
                     '(unsigned-byte 8)
                     :initial-element
                     0))
       (h
         (make-array 8
                     :element-type
                     '(unsigned-byte 32)
                     :initial-contents
                     '(#x6a09e667 #xbb67ae85
                       #x3c6ef372
                       #xa54ff53a
                       #x510e527f
                       #x9b05688c
                       #x1f83d9ab
                       #x5be0cd19)))
       (w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    (replace padded bytes)
    (setf (aref padded (length bytes)) #x80)
    (loop for i from 0 below 8
          do
          (setf (aref padded (- (length padded) 1 i))
                (logand #xff (ash bit-len (* -8 i)))))
    (loop for off from 0 below (length padded) by 64
          do
          (loop for i from 0 below 16
                for j = (+ off (* i 4))
                do
                (setf (aref w i)
                      (logior (ash (aref padded j) 24)
                              (ash (aref padded (+ j 1)) 16)
                              (ash (aref padded (+ j 2)) 8)
                              (aref padded (+ j 3)))))
          (loop for i from 16 below 64
                do
                (let*
                    ((s0
                       (logxor (rotr32 (aref w (- i 15)) 7)
                               (rotr32 (aref w (- i 15)) 18)
                               (ash (aref w (- i 15)) -3)))
                     (s1
                       (logxor (rotr32 (aref w (- i 2)) 17)
                               (rotr32 (aref w (- i 2)) 19)
                               (ash (aref w (- i 2)) -10))))
                  (setf (aref w i) (u32+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
          (let
              ((a (aref h 0)) (b (aref h 1))
               (c (aref h 2))
               (d (aref h 3))
               (e (aref h 4))
               (f (aref h 5))
               (g (aref h 6))
               (hh (aref h 7)))
            (loop for i from 0 below 64
                  do
                  (let*
                      ((s1 (logxor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
                       (ch (logxor (logand e f) (logand (lognot e) g)))
                       (t1 (u32+ hh s1 ch (aref +sha256-k+ i) (aref w i)))
                       (s0 (logxor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
                       (maj (logxor (logand a b) (logand a c) (logand b c)))
                       (t2 (u32+ s0 maj)))
                    (setf hh g g f f e e (u32+ d t1) d c c b b a a (u32+ t1 t2))))
            (setf (aref h 0)
                  (u32+ (aref h 0) a)
                  (aref h 1)
                  (u32+ (aref h 1) b)
                  (aref h 2)
                  (u32+ (aref h 2) c)
                  (aref h 3)
                  (u32+ (aref h 3) d)
                  (aref h 4)
                  (u32+ (aref h 4) e)
                  (aref h 5)
                  (u32+ (aref h 5) f)
                  (aref h 6)
                  (u32+ (aref h 6) g)
                  (aref h 7)
                  (u32+ (aref h 7) hh))))
    (with-output-to-string (s) (loop for x across h do (format s "~8,'0x" x)))))

;;; Curated MCP Skills (Derived from Production Usage Patterns)

(defparameter *mcp-skills*
  (list
    (list :name
          "structural-editing-navigation"
          :description
          "Navigate and mutate Lisp ASTs cleanly without context blowout, safeguards, or redundant reads."
          :files
          (list
            (cons "SKILL.md"
                  "---
name: structural-editing-navigation
description: Navigate and mutate Lisp ASTs cleanly without context blowout, safeguards, or redundant reads.
---

# Structural Editing Navigation & Mutation

## Recommended Patterns
1. **Skeleton first:** Call `read_node` with `mode: \"skeleton\"` and `limit`/`offset` on unfamiliar files or wide trees.
2. **Vertical rays for depth:** Use `read_slice` to inspect deeply nested expressions without lateral sibling bloat.
3. **Target exact child paths or hashnode handles:** Pass `#n...` handles or exact child integer paths (e.g. `[0, 1, 5, 2]`) to `ast_modify` / `ast_remove`.
4. **Trust mutation previews:** `ast_modify`, `ast_remove`, and `ast_relocate` automatically return the updated enclosing parent preview.

## Patterns to Avoid
- Do **not** call `read_node` immediately after `ast_modify` just to verify the edit.
- Do **not** overwrite large parent nodes (`>50` children); target the specific child index instead.
- Do **not** pass multiple top-level forms to a single `ast_modify` `insert`; insert one well-formed S-expression per call.
- Use `ast_remove` with `action: \"delete\"` (not `\"remove\"`).
")))
    (list :name
          "multi-agent-workspace-branching"
          :description
          "Isolate concurrent edits, resolve AST conflicts cleanly, and persist safely with workspace_manage."
          :files
          (list
            (cons "SKILL.md"
                  "---
name: multi-agent-workspace-branching
description: Isolate concurrent edits, resolve AST conflicts cleanly, and persist safely with workspace_manage.
---

# Multi-Agent Workspace Branching

## Recommended Patterns
1. **Fork before multi-step edits:** Call `workspace_manage` (`action: \"fork\"`, `target_id: \"agent-<id>\"`) and pass `workspace_id` to subsequent calls.
2. **Checkpoint risky refactors:** Use `workspace_manage` (`action: \"snapshot\"`) before large structural rewrites.
3. **Inspect before merging:** Run `workspace_status` and `workspace_diff` against `\"default\"` before `workspace_rebase` or `workspace_merge`.
4. **Sync on OCC conflicts:** If `occ_conflict` or `hash_mismatch` occurs, call `read_node` on the enclosing parent once to refresh your view/handle.

## Patterns to Avoid
- Do **not** edit `\"default\"` concurrently from multiple agents without branching.
- Do **not** call `commit_workspace` until edits are merged and tests pass.
")))
    (list :name
          "ast-refactoring-and-analysis"
          :description
          "Combine static analysis tools with semantic AST refactorings for safe code quality improvements."
          :files
          (list
            (cons "SKILL.md"
                  "---
name: ast-refactoring-and-analysis
description: Combine static analysis tools with semantic AST refactorings for safe code quality improvements.
---

# AST Refactoring & Static Analysis

## Recommended Workflow
1. Run `ast_suggest_refactorings` (or `ast_lint` + `ast_complexity_metrics` + `ast_find_duplicates` + `ast_analyze_bindings`) to locate high-value targets.
2. Use `ast_rename` for symbol renames (safely ignores strings and comments).
3. Use `ast_replace_pattern` with `?var` wildcards for API migrations instead of manual `overwrite` loops.
4. Use `ast_extract_variable` and `ast_extract_function` to decompose complex or duplicated subtrees.
"))))
  "Built-in MCP skills catalog following SEP-2640.")

(defun skill-primary-uri (skill) (fmt "skill://~A/SKILL.md" (getf skill :name)))

(defun skill-file-uri (skill rel-path)
  (fmt "skill://~A/~A" (getf skill :name) rel-path))

(defun build-skill-descriptor (skill)
  "Build SEP-2640 skill entry dict with uri, frontmatter, and SHA-256 resources manifest."
  (let
      ((resources
         (coerce
           (loop for (rel . body) in (getf skill :files)
                 for bytes = (sb-ext:string-to-octets body :external-format :utf-8)
                 collect
                 (dict "uri"
                       (skill-file-uri skill rel)
                       "size"
                       (length bytes)
                       "sha256"
                       (sha256-hex body)))
           'vector)))
    (dict "uri"
          (skill-primary-uri skill)
          "frontmatter"
          (dict "name" (getf skill :name) "description" (getf skill :description))
          "resources"
          resources)))

(defun read-skill-resource-uri (uri)
  "Resolve skill://<name>/<rel> to (values content mime-type) or NIL."
  (when (serapeum:string-prefix-p "skill://" uri)
    (let*
        ((rest (subseq uri (length "skill://"))) (slash (position #\/ rest)))
      (when slash
        (let*
            ((sname (subseq rest 0 slash))
             (rel (subseq rest (1+ slash)))
             (skill
               (find sname
                     *mcp-skills*
                     :test
                     #'string=
                     :key
                     (lambda (s)
                       (getf s :name)))))
          (when skill
            (let
                ((pair (assoc rel (getf skill :files) :test #'string=)))
              (when pair (values (cdr pair) "text/markdown")))))))))

(defun list-skill-resources ()
  "Return MCP resource descriptors for all skill files."
  (loop for skill in *mcp-skills*
        append
        (loop for (rel . nil) in (getf skill :files)
              collect
              (dict "uri"
                    (skill-file-uri skill rel)
                    "name"
                    (fmt "~A/~A" (getf skill :name) rel)
                    "description"
                    (getf skill :description)
                    "mimeType"
                    "text/markdown"))))

(pushnew 'list-skill-resources *extra-resource-listers*)

(pushnew 'read-skill-resource-uri *extra-resource-readers*)

(defun handle-skills-list (id params)
  "Handle SEP-2640 skills/list with optional cursor pagination."
  (let*
      ((all (mapcar #'build-skill-descriptor *mcp-skills*))
       (cursor-str (and (hash-table-p params) (href params "cursor")))
       (offset
         (or (and (stringp cursor-str) (ignore-errors (parse-integer cursor-str))) 0))
       (limit 50)
       (total (length all))
       (start (min (max 0 offset) total))
       (end (min (+ start limit) total))
       (res (dict "skills" (coerce (subseq all start end) 'vector))))
    (when (< end total) (setf (gethash "nextCursor" res) (write-to-string end)))
    (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
    (send-result id res)))

(defun handle-skills-get (id params)
  "Handle SEP-2640 skills/get for a single skill URI."
  (let*
      ((uri (and (hash-table-p params) (href params "uri")))
       (skill
         (and (stringp uri)
              (find uri *mcp-skills* :test #'string= :key #'skill-primary-uri))))
    (if (null skill)
      (send-error-with-data id
                            +error-code-invalid-params+
                            (fmt "Skill not found: ~A" uri)
                            (dict "uri" uri))
      (let ((res (build-skill-descriptor skill)))
        (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
        (send-result id res)))))

(defun handle-resources-directory-read
       (id params)
  "Handle optional SEP-2640 resources/directory/read for skill:// prefixes."
  (let*
      ((uri (and (hash-table-p params) (href params "uri")))
       (entries
         (and (stringp uri)
              (remove-if-not
                (lambda (r)
                  (serapeum:string-prefix-p uri (gethash "uri" r)))
                (list-skill-resources)))))
    (if (null entries)
      (send-error-with-data id
                            +error-code-invalid-params+
                            (fmt "Directory not found: ~A" uri)
                            (dict "uri" uri))
      (let
          ((res (dict "resources" (coerce entries 'vector))))
        (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
        (send-result id res)))))