(in-package :structural-editing-mcp-tests)

(deftest test-calver-version-format
         (testing "version string is non-empty and accessible via +version+ and get-version"
                  (ok (stringp +version+))
                  (ok (plusp (length +version+)))
                  (ok (equal (get-version) +version+)))

         (testing "version matches ASDF system component version"
                  (let ((sys (asdf:find-system :structural-editing-mcp nil)))
                    (ok sys)
                    (ok (equal (asdf:component-version sys) +version+))))

         (testing "version parses validly under ASDF without warnings"
                  (ok (asdf:version-satisfies +version+ "0.0.1")))

         (testing "version complies with CalVer schema (YYYY.M.D[.MICRO]) with no leading zeros"
                  (let* ((parts (uiop:split-string +version+ :separator "."))
                         (numbers (mapcar #'parse-integer parts)))
                    (ok (or (= (length parts) 3) (= (length parts) 4)))
                    ;; Check year
                    (ok (>= (first numbers) 2024))
                    ;; Check month (1-12)
                    (ok (<= 1 (second numbers) 12))
                    ;; Check day (1-31)
                    (ok (<= 1 (third numbers) 31))
                    ;; Check no leading zeros in string representations (e.g. "9", not "09")
                    (dolist (p parts)
                      (ok (string= p (write-to-string (parse-integer p)))))))

         (testing "CalVer ordering behaves monotonically"
                  (ok (asdf:version-satisfies +version+ "2024.1.1"))
                  (ok (asdf:version-satisfies "2026.9.18.2" "2026.9.18.1"))
                  (ok (asdf:version-satisfies "2026.9.18.1" "2026.9.18"))
                  (ok (asdf:version-satisfies "2026.9.19" "2026.9.18.99"))))

(deftest test-mcp-initialize-calver
         (testing "initialize response reports exact CalVer version"
                  (let* ((msg (structural-editing-mcp.mcp::dict
                                "jsonrpc" "2.0"
                                "id" 99
                                "method" "initialize"
                                "params" (make-hash-table)))
                         (*standard-output* (make-string-output-stream))
                         (result-str (progn
                                       (structural-editing-mcp.mcp:handle-message msg)
                                       (get-output-stream-string *standard-output*)))
                         (result-json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse result-str))))
                    (let* ((res (gethash "result" result-json))
                           (info (gethash "serverInfo" res)))
                      (ok (equal (gethash "version" info) +version+))))))
