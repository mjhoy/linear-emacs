;;; linear-unit-test.el --- Unit tests for linear.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'linear)

;; Unit tests for query builder (no server needed)

(ert-deftest linear-test-build-query-default ()
  "Test that default filter builds viewer path with completedAt/canceledAt."
  (let* ((filter '(:assignee me))
         (query (linear--build-query filter)))
    (should (string-match-p "viewer" query))
    (should (string-match-p "assignedIssues" query))
    (should (string-match-p "completedAt: { null: true }" query))
    (should (string-match-p "canceledAt: { null: true }" query))))

(ert-deftest linear-test-build-query-states ()
  "Test that states filter builds state name eq/in."
  (let* ((filter-single '(:assignee me :states ("Todo")))
         (query-single (linear--build-query filter-single))
         (filter-multi '(:assignee me :states ("Todo" "In Progress")))
         (query-multi (linear--build-query filter-multi)))
    ;; Single state: eq (escaped quotes for JSON embedding)
    (should (string-match-p "state: { name: { eq: \\\\\"Todo\\\\\" } }" query-single))
    (should-not (string-match-p "completedAt" query-single))
    ;; Multiple states: in
    (should (string-match-p "state: { name: { in:" query-multi))
    (should (string-match-p "\\\\\"Todo\\\\\"" query-multi))
    (should (string-match-p "\\\\\"In Progress\\\\\"" query-multi))
    (should-not (string-match-p "completedAt" query-multi))))

(ert-deftest linear-test-build-query-team ()
  "Test that team filter builds team key eq."
  (let* ((filter '(:assignee me :team "CHO"))
         (query (linear--build-query filter)))
    (should (string-match-p "team: { key: { eq: \\\\\"CHO\\\\\" } }" query))))

(ert-deftest linear-test-build-query-project ()
  "Test that project filter builds project name eq."
  (let* ((filter '(:assignee me :project "My Project"))
         (query (linear--build-query filter)))
    (should (string-match-p "project: { name: { eq: \\\\\"My Project\\\\\" } }" query))))

(ert-deftest linear-test-build-query-specific-assignee ()
  "Test that string assignee uses root issues path."
  (let* ((filter '(:assignee "john@linear.app"))
         (query (linear--build-query filter)))
    (should (string-match-p "issues(filter:" query))
    (should-not (string-match-p "viewer" query))
    (should (string-match-p "assignee: { email: { eq: \\\\\"john@linear.app\\\\\" } }" query))))

(provide 'linear-unit-test)
;;; linear-unit-test.el ends here
