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

(defun linear--get-api-key ()
  "Retrieve linear API key from auth-source."
  (let ((auth (nth 0 (auth-source-search :host "api.linear.app" :requires '(secret)))))
    (funcall (plist-get auth :secret))))

(defun linear-retrieve ()
  "Retrieve from the Linear graphql endpoint."
  (let* ((api-key (linear--get-api-key))
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
                        (message "Got json: %s" result)))
                    )
                (message "Bad status code: %s" status-code))
              )))
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,api-key)
                                      ("Content-Type" . "application/json")))
         (url-request-data "{\"query\": \"{ issues { nodes { id title } } }\" }")
         )
    (url-retrieve "https://api.linear.app/graphql" handler '() t))
  )

(provide 'linear)
;;; linear.el ends here
