;;; linear-test.el --- Tests for linear.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'linear)
(require 'linear-issue)

;; Test configuration
(defvar linear-test-server-process nil
  "Process for the mock Linear server.")

(defvar linear-test-original-api-token nil
  "Original API token to restore after tests.")

(defvar linear-test-original-api-endpoint nil
  "Original API endpoint to restore after tests.")

;; Helper functions for server lifecycle
(defun linear-test--build-server ()
  "Build the mock server (blocking)."
  (message "Building Linear mock server...")
  (let ((result (call-process "cargo" nil "*linear-test-build*" t
                              "build"
                              "--release"
                              "--manifest-path"
                              (expand-file-name "test-server/Cargo.toml" default-directory))))
    (unless (zerop result)
      (error "Failed to build mock server. Check *linear-test-build* buffer for details"))
    (message "Mock server built successfully")))

(defun linear-test--start-server ()
  "Start the mock Linear GraphQL server."
  (when (and linear-test-server-process
             (process-live-p linear-test-server-process))
    (kill-process linear-test-server-process))
  
  ;; Build first (blocking)
  (linear-test--build-server)
  
  ;; Start pre-built server with correct working directory
  (let ((default-directory (expand-file-name "test-server/" default-directory)))
    (setq linear-test-server-process
          (start-process "linear-mock-server"
                         "*linear-test-server*"
                         "cargo"
                         "run"
                         "--release")))
  
  ;; Wait for server to start (much shorter now)
  (message "Starting Linear mock server...")
  (sleep-for 1)
  
  ;; Verify server is responding
  (let ((attempts 0))
    (while (and (< attempts 10)
                (condition-case nil
                    (progn
                      (url-retrieve-synchronously "http://localhost:8080/graphql" nil nil 1)
                      nil)
                  (error t)))
      (setq attempts (1+ attempts))
      (sleep-for 0.5))
    (when (>= attempts 10)
      (error "Mock server failed to start")))
  
  (message "Linear mock server ready"))

(defun linear-test--stop-server ()
  "Stop the mock Linear GraphQL server."
  (when (and linear-test-server-process
             (process-live-p linear-test-server-process))
    (kill-process linear-test-server-process)
    (setq linear-test-server-process nil))
  (message "Mock server stopped"))

(defun linear-test--setup ()
  "Set up test environment."
  ;; Save original configuration
  (setq linear-test-original-api-token linear-api-token)
  (setq linear-test-original-api-endpoint linear-api-endpoint)
  
  ;; Configure for testing
  (setq linear-api-token "test-token")
  (setq linear-api-endpoint "http://localhost:8080/graphql")
  
  ;; Start mock server
  (linear-test--start-server))

(defun linear-test--teardown ()
  "Clean up test environment."
  ;; Stop server
  (linear-test--stop-server)
  
  ;; Restore original configuration
  (setq linear-api-token linear-test-original-api-token)
  (setq linear-api-endpoint linear-test-original-api-endpoint)
  
  ;; Clean up test buffers
  (when (get-buffer "*linear*")
    (kill-buffer "*linear*"))
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\*linear-issue-.*\\*" (buffer-name buffer))
      (kill-buffer buffer))))


;; Test fixtures
(ert-deftest linear-test-retrieve-issues ()
  "Test retrieving assigned issues from mock server."
  (linear-test--setup)
  (unwind-protect
      (progn
        ;; Call linear-retrieve and wait for response
        (linear-retrieve)
        
        ;; Give time for async request to complete
        (sleep-for 1)
        
        ;; Check that buffer was created and populated
        (with-current-buffer "*linear*"
          (should (> (buffer-size) 0))
          (goto-char (point-min))
          
          ;; Should contain our test issues
          (should (search-forward "CHO-26" nil t))
          (should (search-forward "Better style" nil t))
          
          (goto-char (point-min))
          (should (search-forward "CHO-25" nil t))
          (should (search-forward "Work on some improvements" nil t))
          
          (goto-char (point-min))
          (should (search-forward "CHO-18" nil t))
          (should (search-forward "Allow reseting the timer" nil t))))
    (linear-test--teardown)))

(ert-deftest linear-test-show-issue-details ()
  "Test showing issue details from mock server."
  (linear-test--setup)
  (unwind-protect
      (progn
        (progn
          ;; First populate the main buffer
          (linear-retrieve)
          (sleep-for 1)
          
          ;; Navigate to first issue and show details
          (with-current-buffer "*linear*"
            (goto-char (point-min))
            (search-forward "CHO-26")
            (beginning-of-line)
            
            ;; Show issue details
            (linear-show-item-details)
            (sleep-for 1)
            
            ;; Check issue detail buffer was created
            (let ((issue-buffer (get-buffer "*linear-issue-92ddcc64-5765-4a41-980e-49493eb086b3*")))
              (should issue-buffer)
              
              (with-current-buffer issue-buffer
                (should (> (buffer-size) 0))
                (goto-char (point-min))
                
                ;; Check issue details are displayed
                (should (search-forward "Issue: CHO-26 Better style" nil t))
                (should (search-forward "State: Todo 🫡" nil t))
                (should (search-forward "Assignee: mjh@mjhoy.com" nil t))
                (should (search-forward "(No description.)" nil t))
                
                ;; Check comments are displayed
                (should (search-forward "Comments:" nil t))
                (should (search-forward "This is a markdown comment." nil t)))))))
    (linear-test--teardown)))

(ert-deftest linear-test-null-description-handling ()
  "Test that null descriptions are handled properly."
  (linear-test--setup)
  (unwind-protect
      (progn
        (progn
          ;; Get issue with null description (CHO-25)
          (linear-retrieve-issue "f923e500-0bab-4fa1-b5f2-f1144485feeb")
          (sleep-for 1)
          
          (let ((issue-buffer (get-buffer "*linear-issue-f923e500-0bab-4fa1-b5f2-f1144485feeb*")))
            (should issue-buffer)
            
            (with-current-buffer issue-buffer
              (goto-char (point-min))
              ;; Should show "(No description.)" instead of failing
              (should (search-forward "(No description.)" nil t))))))
    (linear-test--teardown)))

(ert-deftest linear-test-issue-with-description ()
  "Test that real descriptions are displayed properly."
  (linear-test--setup)
  (unwind-protect
      (progn
        (progn
          ;; Get issue with real description (CHO-18)
          (linear-retrieve-issue "adf20c5e-5a52-4906-8706-fdf825c7f891")
          (sleep-for 1)
          
          (let ((issue-buffer (get-buffer "*linear-issue-adf20c5e-5a52-4906-8706-fdf825c7f891*")))
            (should issue-buffer)
            
            (with-current-buffer issue-buffer
              (goto-char (point-min))
              ;; Should show the actual description
              (should (search-forward "A test description." nil t))))))
    (linear-test--teardown)))

;; Utility to run all tests
(defun linear-run-tests ()
  "Run all Linear tests with proper setup/teardown."
  (interactive)
  (ert-run-tests-interactively "linear-test-"))

(provide 'linear-test)
;;; linear-test.el ends here
