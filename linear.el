;;; linear.el --- Linear UI          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mikey Hoy

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

;; Silences warning about free variable.
(defvar url-http-end-of-headers)

(defcustom linear-api-token nil
  "Your Linear API token."
  :type 'string
  :group 'linear)

(defcustom linear-api-use-auth-source t
  "If non-nil, use auth-source to retrieve linear-api-token key.

The secret should be stored in the format

  machine api.linear.app password <API_KEY>

in your `auth-sources' file."
  :type 'boolean
  :group 'linear)

(defun linear--get-api-key-from-auth-source ()
  "Retrieve linear API key from auth-source."
  (let ((auth (nth 0 (auth-source-search :host "api.linear.app" :requires '(secret)))))
    (funcall (plist-get auth :secret))))

(when (and linear-api-use-auth-source (not linear-api-token))
  (setq linear-api-token (linear--get-api-key-from-auth-source)))

(defun linear-retrieve ()
  "Retrieve from the Linear graphql endpoint."
  (let* ((linear-buffer (linear--get-or-create-buf))
         (handler
          (lambda (_)
            (let ((status-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))))
              (if (eq status-code 200)
                  (progn
                    (goto-char url-http-end-of-headers)
                    (let ((json-object-type 'plist)
                          (json-key-type 'symbol)
                          (json-array-type 'vector))
                      (let ((result (json-read)))
                        (linear--populate-buffer result)
                        ))
                    )
                (progn
                  ;; (message "Bad status code: %s" status-code)
                  ;; (message "Error: %s" (buffer-substring (point) (point-max)))
                  (goto-char url-http-end-of-headers)
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector))
                    (let ((result (json-read)))
                      (linear--populate-buffer-err result)
                      ))
                  ))
              )))
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,linear-api-token)
                                      ("Content-Type" . "application/json")))
         (url-request-data "{\"query\": \"{ viewer { assignedIssues(filter: { completedAt: { null: true } }) { nodes { id title state { name color } } } } }\" }")
         )
    (with-current-buffer linear-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "😽 Loading 😺")
      (setq buffer-read-only t)
      )
    (linear--switch-to-buf)
    (url-retrieve "https://api.linear.app/graphql" handler '() t)
    )
  )

;; Debugging
(defvar linear--last-response)

(defun linear--resp-get-nodes (response)
  "Takes a Linear RESPONSE and return a list of nodes."
  (let* ((data (cdr response))
         (viewer (cdr (assq 'viewer data)))
         (assignedIssues (cdr (assq 'assignedIssues viewer)))
         (nodes (car (cdr (assq 'nodes assignedIssues)))))
    nodes))

(defun linear--resp-get-errs (response)
  "Takes a Linear error RESPONSE and return a list of nodes."
  (car (cdr response)))

(defun linear--populate-buffer (response)
  "Takes a Linear RESPONSE and writes to the buffer."
  (setq linear--last-response response)
  (with-current-buffer (linear--get-or-create-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (seq-do (lambda (item)
              (let* ((title (plist-get item 'title))
                     (state (plist-get item 'state))
                     (state-name (plist-get state 'name))
                     )
                (insert (format "* [%s] %s" (decode-coding-string state-name 'utf-8) (decode-coding-string title 'utf-8)))
                (newline)))
            (linear--resp-get-nodes response))
    (setq buffer-read-only t)
    ))

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


(defun linear--get-or-create-buf ()
  "Get the linear buffer if it exists, or create it."
  (let ((buffer (or
                 (get-buffer "*linear*")
                 (progn
                   (let ((buffer (generate-new-buffer "*linear*")))
                     (set-buffer-major-mode buffer)
                     (with-current-buffer buffer
                       (read-only-mode t)
                       )
                     buffer)))))
    buffer))

(defun linear--switch-to-buf ()
  "Switch to the linear buffer."
  (let* ((buffer (linear--get-or-create-buf))
         (window (display-buffer buffer '(display-buffer-at-bottom . nil))))
    (select-window window)
    (view-mode t)
    ))

(defun linear ()
  "Open the Linear UI."
  (interactive)
  (linear-retrieve)
  )

(provide 'linear)
;;; linear.el ends here
