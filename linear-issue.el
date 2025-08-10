;;; linear-issue.el --- Linear issue UI    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikey Hoy

;; Author: Mikey Hoy <mjh@mjhoy.com>
;; Keywords: linear
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides detailed issue view functionality for Linear.

;;; Code:

(require 'url)
(require 'url-http)
(require 'parse-time)

;; Silences warning about free variable.
(defvar url-http-end-of-headers)
(defvar linear-api-token)

(defvar linear-issue-mode-map nil
  "Keymap for Linear issue detail major mode.")
(unless linear-issue-mode-map
  (setq linear-issue-mode-map (make-sparse-keymap))
  (define-key linear-issue-mode-map (kbd "q") 'linear-issue-quit)
  (define-key linear-issue-mode-map (kbd "o") 'linear-issue-open-browser)
  (define-key linear-issue-mode-map (kbd "g") 'linear-issue-refresh)
  (define-key linear-issue-mode-map (kbd "p") 'previous-line)
  (define-key linear-issue-mode-map (kbd "n") 'next-line)
  )
(fset 'linear-issue-mode-map linear-issue-mode-map)

;;;###autoload
(define-derived-mode linear-issue-mode special-mode
  "linear-issue"
  "Major mode for displaying Linear issue details."
  (use-local-map linear-issue-mode-map)
  )

(defun linear-retrieve-issue (issue-id)
  "Retrieve detailed information for a Linear issue."
  (let* ((issue-buffer (linear--get-or-create-issue-buf issue-id))
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,linear-api-token)
                                      ("Content-Type" . "application/json")))
         (query (format "{\"query\": \"{ issue(id: \\\"%s\\\") { id identifier title url description state { name color } assignee { name email } labels { nodes { name color } } comments { nodes { body user { name } createdAt } } } }\" }" issue-id))
         (url-request-data query)
         (handler
          (lambda (_)
            (let ((status-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))))
              (if (eq status-code 200)
                  (progn
                    (goto-char url-http-end-of-headers)
                    (let ((result (json-parse-buffer :object-type 'plist :array-type 'array)))
                      (linear--populate-issue-buffer result issue-id)))
                (progn
                  (goto-char url-http-end-of-headers)
                  (let ((result (json-parse-buffer :object-type 'plist :array-type 'vector)))
                    (message "Error fetching issue details: %s" (plist-get (aref (plist-get result :errors) 0) :message)))))))))
    (with-current-buffer issue-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "😽 Loading 😺")
      (setq buffer-read-only t)
      )
    (linear--switch-to-issue-buf issue-id)
    (url-retrieve linear-api-endpoint handler '() t)
    ))

(defun linear--switch-to-issue-buf (issue-id)
  "Switch to the linear issue buffer."
  (let* ((buffer (linear--get-or-create-issue-buf issue-id))
         (window (display-buffer buffer)))
    (select-window window)
    ))

(defun linear--get-or-create-issue-buf (issue-id)
  "Get or create buffer for Linear issue details."
  (let* ((buffer-name (format "*linear-issue-%s*" issue-id))
         (buffer (or (get-buffer buffer-name)
                     (progn
                       (let ((buffer (generate-new-buffer buffer-name)))
                         (with-current-buffer buffer
                           (linear-issue-mode)
                           (setq-local linear-current-issue-id issue-id))
                         buffer)))))
    buffer))

(defun linear--populate-issue-buffer (response issue-id)
  "Populate issue detail buffer with RESPONSE data."
  (let* ((data (plist-get response :data))
         (issue (plist-get data :issue))
         (buffer (linear--get-or-create-issue-buf issue-id)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      
      ;; Issue header
      (let* ((identifier (plist-get issue :identifier))
             (title (plist-get issue :title))
             (url (plist-get issue :url))
             (state (plist-get issue :state))
             (state-name (plist-get state :name))
             (assignee (plist-get issue :assignee))
             (assignee-name (when assignee (plist-get assignee :name)))
             (description (plist-get issue :description))
             (labels (plist-get issue :labels))
             (label-nodes (when labels (plist-get labels :nodes)))
             (comments (plist-get issue :comments))
             (comment-nodes (when comments (plist-get comments :nodes))))
        
        ;; Store issue data for keybindings
        (setq-local linear-current-issue issue)
        
        ;; Header
        (insert (format "Issue: %s %s\n" 
                       (decode-coding-string identifier 'utf-8)
                       (decode-coding-string title 'utf-8)))
        (insert (format "State: %s\n" (decode-coding-string state-name 'utf-8)))
        (when assignee-name
          (insert (format "Assignee: %s\n" (decode-coding-string assignee-name 'utf-8))))
        
        ;; Labels
        (when label-nodes
          (insert "Labels: ")
          (seq-do (lambda (label)
                    (let ((label-name (plist-get label :name)))
                      (insert (format "%s " (decode-coding-string label-name 'utf-8)))))
                  label-nodes)
          (insert "\n"))
        
        (insert (format "URL: %s\n\n" url))
        
        (if (and description (not (eq :null description)) (not (string-empty-p description)))
            (progn
              (insert "Description:\n")
              (insert (decode-coding-string description 'utf-8)))
          (insert "(No description.)"))
        (insert "\n\n")
        
        (when comment-nodes
          (insert "Comments:\n\n")
          (seq-do (lambda (comment)
                    (let* ((body (plist-get comment :body))
                           (user (plist-get comment :user))
                           (user-name (when user (plist-get user :name)))
                           (created-at (plist-get comment :createdAt)))
                      (insert (format "[%s] %s:\n" 
                                     (linear--format-timestamp created-at)
                                     (decode-coding-string user-name 'utf-8)))
                      (insert (format "%s\n\n" (decode-coding-string body 'utf-8)))))
                  comment-nodes)))
      
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    ;; Switch to the issue buffer
    (let ((window (display-buffer buffer)))
      (select-window window))))

(defun linear--format-timestamp (iso8601-string)
  "Format an ISO8601 timestamp string for display in local timezone."
  (when iso8601-string
    (let* ((parsed-time (parse-iso8601-time-string iso8601-string)))
      (format-time-string "%Y-%m-%d %H:%M" parsed-time))))

(defun linear-issue-quit ()
  "Quit the issue detail buffer and return to main linear buffer."
  (interactive)
  (quit-window)
  (when (get-buffer "*linear*")
    (switch-to-buffer "*linear*")))

(defun linear-issue-open-browser ()
  "Open current issue in browser."
  (interactive)
  (when (bound-and-true-p linear-current-issue)
    (let ((url (plist-get linear-current-issue :url)))
      (browse-url url))))

(defun linear-issue-refresh ()
  "Refresh current issue details."
  (interactive)
  (when (bound-and-true-p linear-current-issue-id)
    (linear-retrieve-issue linear-current-issue-id)))

(defun linear-show-item-details ()
  "Show detailed view of the item at point."
  (interactive)
  (let* ((item (linear--get-item-at-point))
         (issue-id (plist-get item :id)))
    (when issue-id
      (linear-retrieve-issue issue-id))))

(provide 'linear-issue)
;;; linear-issue.el ends here
