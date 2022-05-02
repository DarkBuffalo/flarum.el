;;; flarum.el --- Flarum api                         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Matthias David

;; Author: Matthias David <darkbuffalo@gnu.re>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'request)
(require 'subr-x)
(require 'json)


(defgroup flarum nil
  "Client for flarum network api."
  :group 'Applications)

(defcustom flarum-client-api-url "https://emacs.gnu.re/public/api/discussions" "API url."
  :type '(string)
  :group 'flarum)

;;;; Variables



(defvar flarum-client-works nil "Data structure for flarum discussions.")

;;;###autoload
(defun flarum-client-retrieve-works ()
  "Retrieve works from flarum network."
  (interactive)
  ;;(flarum-client-set-prompt-api-token)
  ;; (if poet-client-enable-logs
  ;;     (progn (custom-set-variables '(request-log-level 'blather)
  ;;                                  '(request-message-level 'blather)))
  ;;   (progn (custom-set-variables '(request-log-level -1)
  ;;                                '(request-message-level -1))))
  (message "Retrieving Discussions list ...")
  (request
   flarum-client-api-url
   :type "GET"
   :headers `(("Content-Type" . "application/json"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq flarum-client-works data)
               (flarum-client-works-list-popup (flarum-client-parse-works-response data))))))


	 (defun flarum-client-parse-works-response (discs-json-response)
		 "Parse discussions json response and convert it to a list of vector.
DISCS-JSON-RESPONSE api response of $API_URL/discussions"
		 (let ((index 0))
			 (mapcar (lambda (disc)
								 (append (list (cl-incf index) (flarum-client-parse-works-extract-values disc))))
							 discs-json-response)))

(defun flarum-client-parse-works-extract-values (disc)
  "Extract values from a single DISC entry.
WORK work entry"
  (vector (assoc-default 'id disc)
					;; (assoc-default 'title (assoc-default 'attributes disc))
					;; (assoc-default 'shareUrl (assoc-default 'attributes disc))
					))

(define-derived-mode flarum-client-mode tabulated-list-mode "flarum-mode"
  "Major mode for flarum UI menu of publised works."
  (define-key tabulated-list-mode-map (kbd "RET") (lambda () (interactive) (poet-client-get-selected-work-from-url)))
  (use-local-map tabulated-list-mode-map)
  (setq tabulated-list-format [("id" 3 t)
                               ;; ("title" 20 nil)
                               ;; ("attributes" 20 t)
															 ])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun flarum-client-works-list-popup (discs-list)
  "List published works in a popup.
DISCS-LIST list of published works"
  (pop-to-buffer "*Flarum Discussions*" nil)
  (flarum-client-mode)
  (setq tabulated-list-entries discs-list)
  (tabulated-list-print t))


(provide 'flarum)
;;; flarum.el ends here
