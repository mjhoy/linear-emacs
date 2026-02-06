;;; linear.el --- Linear UI          -*- lexical-binding: t; -*-

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

;; Provides a basic UI to Linear.
;;
;; Requires a Linear API key.  Add this to your .authinfo:
;;
;; machine api.linear.app password <API_KEY>

;;; Code:

(require 'url)
(require 'url-http)
(require 'auth-source)
(require 'linear-issue)

(declare-function org-link-set-parameters "org")
(declare-function org-link-store-props "org")

(defun org-linear-store-link ()
  "Store the link to the linear issue at point."
  (when (equal major-mode 'linear-mode)
    (let* ((item (linear--get-item-at-point))
          (url (plist-get item :url))
          (title (plist-get item :title))
          (identifier (plist-get item :identifier)))

      (org-link-store-props
       :type "linear"
       :link url
       :description (format "[%s] %s" (decode-coding-string identifier 'utf-8) (decode-coding-string title 'utf-8)))
      )))

(with-eval-after-load 'org
  (org-link-set-parameters
   "linear" :store 'org-linear-store-link))

;; Silences warning about free variable.
(defvar url-http-end-of-headers)

(defcustom linear-api-token nil
  "Your Linear API token."
  :type 'string
  :group 'linear)

(defcustom linear-api-use-auth-source t
  "If non-nil, use auth-source to retrieve `linear-api-token' key.

The secret should be stored in the format

  machine api.linear.app password <API_KEY>

in your `auth-sources' file."
  :type 'boolean
  :group 'linear)

(defcustom linear-api-endpoint "https://api.linear.app/graphql"
  "Linear GraphQL API endpoint URL."
  :type 'string
  :group 'linear)

(defcustom linear-views
  '((:name "My Issues"
     :filter (:assignee me)))
  "List of saved Linear views.
Each element is a plist with keys :name and :filter.
The :filter plist supports :assignee, :states, :team, and :project."
  :type '(repeat plist)
  :group 'linear)

(defun linear--get-api-key-from-auth-source ()
  "Retrieve linear API key from auth-source."
  (let ((auth (nth 0 (auth-source-search :host "api.linear.app" :requires '(secret)))))
    (funcall (plist-get auth :secret))))

(when (and linear-api-use-auth-source (not linear-api-token))
  (setq linear-api-token (linear--get-api-key-from-auth-source)))

(defvar-local linear-current-view nil
  "The currently active Linear view in this buffer.")

(defconst linear--issue-node-fields
  "id identifier url title state { name color }"
  "GraphQL fields to fetch for issue nodes.")

(defun linear--build-filter-string (filter)
  "Build the inner GraphQL filter content from a FILTER plist.
String values are escaped for embedding inside a JSON string."
  (let ((clauses '())
        (states (plist-get filter :states))
        (team (plist-get filter :team))
        (project (plist-get filter :project))
        (assignee (plist-get filter :assignee)))
    (cond
     ((null states)
      (push "completedAt: { null: true }" clauses)
      (push "canceledAt: { null: true }" clauses))
     ((= (length states) 1)
      (push (format "state: { name: { eq: \\\"%s\\\" } }" (car states)) clauses))
     (t
      (push (format "state: { name: { in: [%s] } }"
                     (mapconcat (lambda (s) (format "\\\"%s\\\"" s)) states ", "))
            clauses)))
    (when team
      (push (format "team: { key: { eq: \\\"%s\\\" } }" team) clauses))
    (when project
      (push (format "project: { name: { eq: \\\"%s\\\" } }" project) clauses))
    (when (stringp assignee)
      (push (format "assignee: { email: { eq: \\\"%s\\\" } }" assignee) clauses))
    (mapconcat #'identity (nreverse clauses) ", ")))

(defun linear--build-query (filter)
  "Build the full GraphQL query JSON string from a FILTER plist."
  (let* ((assignee (plist-get filter :assignee))
         (filter-string (linear--build-filter-string filter))
         (fields linear--issue-node-fields)
         (inner (if (eq assignee 'me)
                    (format "viewer { assignedIssues(filter: { %s }) { nodes { %s } } }"
                            filter-string fields)
                  (format "issues(filter: { %s }) { nodes { %s } }"
                          filter-string fields))))
    (format "{\"query\": \"{ %s }\" }" inner)))

(defvar linear-mode-map nil
  "Keymap for Linear major mode.")
(unless linear-mode-map
  (setq linear-mode-map (make-sparse-keymap))
  (define-key linear-mode-map (kbd "g") 'linear-refresh)
  (define-key linear-mode-map (kbd "RET") 'linear-show-item-details)
  (define-key linear-mode-map (kbd "o") 'linear-open-item)
  (define-key linear-mode-map (kbd "C-w") 'linear-kill-region)
  (define-key linear-mode-map (kbd "p") 'previous-line)
  (define-key linear-mode-map (kbd "n") 'next-line)
  (define-key linear-mode-map (kbd "s") 'linear-switch-view)
  )
(fset 'linear-mode-map linear-mode-map)

;;;###autoload
(define-derived-mode linear-mode special-mode
  "linear"
  "Major mode for displaying Linear tickets."
  (use-local-map linear-mode-map)
  )

(defun linear--get-or-create-buf ()
  "Get the linear buffer if it exists, or create it."
  (let ((buffer (or
                 (get-buffer "*linear*")
                 (progn
                   (let ((buffer (generate-new-buffer "*linear*")))
                     (with-current-buffer buffer
                       (linear-mode)
                       )
                     buffer)))))
    buffer))

(defun linear-refresh ()
  "Refresh the Linear buffer."
  (interactive)
  (linear-retrieve linear-current-view))

(defun linear-retrieve (&optional view)
  "Retrieve from the Linear graphql endpoint.
VIEW is a view plist from `linear-views'; defaults to the first entry."
  (let* ((view (or view (car linear-views)))
         (filter (plist-get view :filter))
         (linear-buffer (linear--get-or-create-buf))
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,linear-api-token)
                                      ("Content-Type" . "application/json")))
         (url-request-data (linear--build-query filter))
         (handler
          (lambda (_)
            (let ((status-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))))
              (if (eq status-code 200)
                  (progn
                    (goto-char url-http-end-of-headers)
                    (let ((result (json-parse-buffer :object-type 'plist :array-type 'array)))
                      (linear--populate-buffer result)))
                (progn
                  (goto-char url-http-end-of-headers)
                  (let ((result (json-parse-buffer :object-type 'plist :array-type 'vector)))
                    (linear--populate-buffer-err result))
                  )))
            )))
    (with-current-buffer linear-buffer
      (setq linear-current-view view)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "😽 Loading 😺")
      (setq buffer-read-only t)
      )
    (linear--switch-to-buf)
    (url-retrieve linear-api-endpoint handler '() t)
    ))

;; Debugging
(defvar linear--last-response)

(defun linear--resp-get-nodes (response)
  "Takes a Linear RESPONSE and return a list of nodes."
  (let* ((data (cdr response))
         (viewer (cdr (assq :viewer data)))
         (assignedIssues (cdr (assq :assignedIssues viewer)))
         (nodes (car (cdr (assq :nodes assignedIssues)))))
    nodes))

(defun linear--resp-get-nodes-from-issues (response)
  "Takes a Linear RESPONSE using root issues query and return a list of nodes."
  (let* ((data (plist-get response :data))
         (issues (plist-get data :issues))
         (nodes (plist-get issues :nodes)))
    nodes))

(defun linear--resp-get-errs (response)
  "Takes a Linear error RESPONSE and return a list of nodes."
  (car (cdr response)))

(defun linear--populate-buffer (response)
  "Takes a Linear RESPONSE and writes to the buffer."
  (setq linear--last-response response)
  (with-current-buffer (linear--get-or-create-buf)
    (let* ((view linear-current-view)
           (filter (and view (plist-get view :filter)))
           (assignee (and filter (plist-get filter :assignee)))
           (nodes (if (eq assignee 'me)
                      (linear--resp-get-nodes response)
                    (linear--resp-get-nodes-from-issues response))))
      (setq buffer-read-only nil)
      (erase-buffer)
      (when view
        (setq header-line-format
              (format " Linear: %s" (plist-get view :name))))
      (seq-do (lambda (item)
                (let* ((title (plist-get item :title))
                       (identifier (plist-get item :identifier))
                       (state (plist-get item :state))
                       (state-name (plist-get state :name))
                       (orig-position (point)))
                  (insert (format "* [%s] (%s) %s" (decode-coding-string identifier 'utf-8) (decode-coding-string state-name 'utf-8) (decode-coding-string title 'utf-8)))
                  (put-text-property orig-position (point) 'linear-item item)
                  (newline)))
              nodes)
      (setq buffer-read-only t))))

(defun linear-kill-region ()
  "If point is on a linear item, copy the URL."
  (interactive)
  (let ((item (linear--get-item-at-point)))
    (if item
        (progn
          (let ((url (plist-get item :url)))
            (kill-new url)
            (message "%s" url)))
      (kill-region (region-beginning) (region-end)))))

(defun linear-open-item ()
  "Open the item at point in browser."
  (interactive)
  (let* ((item (linear--get-item-at-point))
         (url (plist-get item :url)))
    (browse-url url)
    ))

(defun linear--get-item-at-point ()
  "Get the Linear item at point."
  (get-text-property (point) 'linear-item))

(defun linear--populate-buffer-err (response)
  "Takes a Linear error RESPONSE and writes to the buffer."
  (setq linear--last-response response)
  (with-current-buffer (linear--get-or-create-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (seq-do (lambda (item)
              (let* ((err-message (plist-get item 'message)))
                (insert (format "[Error from Linear API] %s" (decode-coding-string err-message 'utf-8)))
                (newline)))
            (linear--resp-get-errs response))
    (setq buffer-read-only t)
    ))

(defun linear--switch-to-buf ()
  "Switch to the linear buffer."
  (let* ((buffer (linear--get-or-create-buf))
         (window (display-buffer buffer '(display-buffer-at-bottom . nil))))
    (select-window window)
    ))

(defun linear-switch-view ()
  "Switch to a different Linear saved view."
  (interactive)
  (let* ((names (mapcar (lambda (v) (plist-get v :name)) linear-views))
         (chosen (completing-read "Linear view: " names nil t))
         (view (seq-find (lambda (v) (string= (plist-get v :name) chosen)) linear-views)))
    (when view
      (linear-retrieve view))))

;;;###autoload
(defun linear ()
  "Open the Linear UI."
  (interactive)
  (linear-retrieve)
  )

(provide 'linear)
;;; linear.el ends here
