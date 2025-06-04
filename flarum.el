;;; flarum.el --- Flarum unofficial tool for Emacs -*- lexical-binding: t; -*-

;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (request "0.3.2") (transient "0.3.0"))
;; Author: Matthias David <darkbuffalo@gnu.re>

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2022-2023, Matthias David
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; An Emacs client for Flarum forums.

;;; Code:

(require 'request)
(require 'transient)
(require 'cl-lib)
(require 'auth-source)

;;; Customization

(defgroup flarum nil
  "Flarum client for Emacs."
  :group 'tools)

(defcustom flarum-api-endpoint "https://emacs.gnu.re/public/api"
  "The base URL for the Flarum API endpoint."
  :type 'string
  :group 'flarum)

(defcustom flarum-use-auth-source t
  "Whether to use auth-source for credential storage."
  :type 'boolean
  :group 'flarum)

;;; API Endpoints

(defun flarum-api-url (path)
  "Construct API URL from PATH."
  (concat flarum-api-endpoint path))

(defun flarum-api-discussions ()
  "Return the discussions API endpoint."
  (flarum-api-url "/discussions"))

(defun flarum-api-tags ()
  "Return the tags API endpoint."
  (flarum-api-url "/tags"))

(defun flarum-api-posts ()
  "Return the posts API endpoint."
  (flarum-api-url "/posts"))

(defun flarum-api-auth ()
  "Return the auth API endpoint."
  (flarum-api-url "/token"))

;;; Variables

(defvar flarum-discs '()
  "List of discussions displayed in the *flarum* buffer.")

(defvar flarum-auth-token nil
  "Authentication token for the Flarum API.")

(defvar flarum-current-discussion-id nil
  "ID of the currently selected discussion.")

(defvar flarum--edit-buffer nil
  "Buffer used for editing content.")

(defvar flarum--edit-callback nil
  "Callback function for edit completion.")

;;; Faces

(defface flarum-title-face
  '((t :inherit font-lock-type-face))
  "Face used for the discussion title.")

(defface flarum-author-face
  '((t :inherit font-lock-comment-face))
  "Face used for post authors.")

(defface flarum-date-face
  '((t :inherit font-lock-comment-face))
  "Face used for dates.")

;;; Structs

(cl-defstruct (flarum-disc (:constructor flarum--create-disc)
                           (:copier nil))
  "Metadata for a Flarum discussion."
  (id 0 :read-only t)
  (title "" :read-only t)
  (slug "" :read-only t)
  (shareUrl "" :read-only t))

;;; Mode Definition

(defvar flarum-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'flarum-help)
    (define-key map (kbd "o") #'flarum-open)
    (define-key map (kbd "n") #'flarum-new-discussion)
    (define-key map (kbd "r") #'flarum-reply)
    (define-key map (kbd "v") #'flarum-view-discussion)
    (define-key map (kbd "l") #'flarum-login)
    (define-key map (kbd "L") #'flarum-logout)
    (define-key map (kbd "g") #'flarum-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `flarum-mode' buffers.")

(define-derived-mode flarum-mode tabulated-list-mode "Flarum"
  "Major mode for browsing Flarum discussions.
\\{flarum-mode-map}"
  (setq tabulated-list-format
        [("ID" 5 t)
         ("Title" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("ID" . t))
  (add-hook 'tabulated-list-revert-hook #'flarum-refresh nil t))

;;; Transient

(transient-define-prefix flarum-help ()
  "Help transient for flarum mode."
  ["Navigation"
   ("o" "Open in browser" flarum-open)
   ("v" "View discussion" flarum-view-discussion)
   ("g" "Refresh" flarum-refresh)
   ("q" "Quit" quit-window)]
  ["Writing"
   ("n" "New discussion" flarum-new-discussion)
   ("r" "Reply to discussion" flarum-reply)]
  ["Authentication"
   ("l" "Login" flarum-login)
   ("L" "Logout" flarum-logout)])

;;; Authentication

(defun flarum--get-credentials ()
  "Get credentials from auth-source or prompt user."
  (if flarum-use-auth-source
      (let ((auth (car (auth-source-search
                        :host (url-host (url-generic-parse-url flarum-api-endpoint))
                        :require '(:user :secret)
                        :create t))))
        (when auth
          (list (plist-get auth :user)
                (funcall (plist-get auth :secret)))))
    (list (read-string "Username: ")
          (read-passwd "Password: "))))

(defun flarum-login ()
  "Login to Flarum."
  (interactive)
  (let* ((credentials (flarum--get-credentials))
         (username (car credentials))
         (password (cadr credentials)))
    (when (and username password)
      (request (flarum-api-auth)
        :type "POST"
        :data (json-encode `(("identification" . ,username)
                            ("password" . ,password)))
        :headers '(("Content-Type" . "application/json"))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq flarum-auth-token (alist-get 'token data))
                    (message "Login successful!")
                    (flarum-refresh)))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Login error: %s" error-thrown)))))))

(defun flarum-logout ()
  "Logout from Flarum."
  (interactive)
  (setq flarum-auth-token nil)
  (message "Logged out!"))

(defun flarum--auth-headers ()
  "Return authentication headers if logged in."
  (when flarum-auth-token
    `(("Authorization" . ,(concat "Token " flarum-auth-token)))))

;;; Creating Discussions

(defun flarum-new-discussion ()
  "Create a new discussion."
  (interactive)
  (unless flarum-auth-token
    (call-interactively #'flarum-login))

  (when flarum-auth-token
    (let ((title (read-string "Discussion title: ")))
      (when (and title (not (string-empty-p title)))
        (flarum--edit-content
         "Enter discussion content:"
         (lambda (content)
           (when content
             (flarum--create-discussion title content))))))))

(defun flarum--create-discussion (title content)
  "Create a discussion with TITLE and CONTENT."
  (request (flarum-api-discussions)
    :type "POST"
    :data (json-encode
           `(("data" . (("type" . "discussions")
                       ("attributes" . (("title" . ,title)
                                       ("content" . ,content)))))))
    :headers (append '(("Content-Type" . "application/json"))
                     (flarum--auth-headers))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Discussion created successfully!")
                (flarum-refresh)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error creating discussion: %s" error-thrown)))))

;;; Replying to Discussions

(defun flarum-reply ()
  "Reply to the selected discussion."
  (interactive)
  (unless flarum-auth-token
    (call-interactively #'flarum-login))

  (when flarum-auth-token
    (let ((discussion-id (flarum--get-current-discussion-id)))
      (if discussion-id
          (flarum--edit-content
           "Enter your reply:"
           (lambda (content)
             (when content
               (flarum--create-post discussion-id content))))
        (message "No discussion selected")))))

(defun flarum--get-current-discussion-id ()
  "Get the ID of the currently selected discussion."
  (when (derived-mode-p 'flarum-mode)
    (let ((entry (tabulated-list-get-entry)))
      (when entry
        (aref entry 0)))))

(defun flarum--create-post (discussion-id content)
  "Create a post with CONTENT in discussion DISCUSSION-ID."
  (request (flarum-api-posts)
    :type "POST"
    :data (json-encode
           `(("data" . (("type" . "posts")
                       ("attributes" . (("content" . ,content)))
                       ("relationships" .
                        (("discussion" .
                          (("data" .
                            (("type" . "discussions")
                             ("id" . ,discussion-id)))))))))))
    :headers (append '(("Content-Type" . "application/json"))
                     (flarum--auth-headers))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Reply posted successfully!")
                (flarum-view-discussion)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error posting reply: %s" error-thrown)))))

;;; Content Editing

(defun flarum--edit-content (prompt callback)
  "Open an edit buffer with PROMPT and call CALLBACK with the result."
  (let ((buffer (get-buffer-create "*Flarum Edit*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert (format ";; %s\n" prompt))
      (insert ";; Type C-c C-c to submit, C-c C-k to cancel\n\n")
      (goto-char (point-max))
      (setq-local flarum--edit-callback callback)
      (local-set-key (kbd "C-c C-c") #'flarum--finish-edit)
      (local-set-key (kbd "C-c C-k") #'flarum--cancel-edit))
    (switch-to-buffer buffer)))

(defun flarum--finish-edit ()
  "Finish editing and call the callback."
  (interactive)
  (let ((content (buffer-substring-no-properties
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line 2)
                    (point))
                  (point-max)))
        (callback flarum--edit-callback))
    (kill-buffer)
    (when (and callback (not (string-empty-p (string-trim content))))
      (funcall callback content))))

(defun flarum--cancel-edit ()
  "Cancel editing."
  (interactive)
  (kill-buffer)
  (message "Edit cancelled"))

;;; Viewing Discussions

(defun flarum-view-discussion ()
  "View posts in the selected discussion."
  (interactive)
  (let ((discussion-id (flarum--get-current-discussion-id)))
    (when discussion-id
      (flarum--fetch-discussion-posts discussion-id))))

(defun flarum--fetch-discussion-posts (discussion-id)
  "Fetch and display posts for DISCUSSION-ID."
  (request (concat (flarum-api-discussions) "/" discussion-id)
    :params '(("include" . "posts,posts.user"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((discussion (alist-get 'data data))
                       (included (alist-get 'included data))
                       (title (alist-get 'title (alist-get 'attributes discussion))))
                  (flarum--display-posts title included))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error loading posts: %s" error-thrown)))))

(defun flarum--display-posts (title posts)
  "Display POSTS with TITLE in a dedicated buffer."
  (let ((buffer (get-buffer-create "*Flarum Discussion*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'flarum-title-face))
        (insert "\n" (make-string (length title) ?=) "\n\n")

        (dolist (post posts)
          (when (equal (alist-get 'type post) "posts")
            (let* ((attrs (alist-get 'attributes post))
                   (content (alist-get 'contentHtml attrs))
                   (author (alist-get 'username attrs "Anonymous"))
                   (date (alist-get 'createdAt attrs)))
              (insert (propertize (format "%s - %s\n" author (or date "Unknown date"))
                                  'face 'flarum-author-face))
              (insert (or content "No content") "\n")
              (insert (make-string 40 ?-) "\n\n"))))

        (goto-char (point-min))
        (special-mode)))
    (switch-to-buffer buffer)))

;;; Opening in Browser

(defun flarum-open ()
  "Open the selected discussion in a web browser."
  (interactive)
  (let ((url (tabulated-list-get-id)))
    (when url
      (browse-url url))))

;;; Buffer Management

(defun flarum--format-entry (disc)
  "Format a DISC for display in tabulated list."
  (list (flarum-disc-shareUrl disc)
        (vector (number-to-string (flarum-disc-id disc))
                (propertize (flarum-disc-title disc) 'face 'flarum-title-face))))

(defun flarum-draw-buffer ()
  "Draw the discussions buffer."
  (setq tabulated-list-entries
        (mapcar #'flarum--format-entry flarum-discs))
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;; API Functions

(defun flarum--fetch-discussions (callback)
  "Fetch discussions from API and call CALLBACK with results."
  (request (flarum-api-discussions)
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((discussions (alist-get 'data data)))
                  (funcall callback
                           (mapcar (lambda (disc)
                                     (flarum--create-disc
                                      :id (string-to-number (alist-get 'id disc))
                                      :title (alist-get 'title (alist-get 'attributes disc))
                                      :slug (alist-get 'slug (alist-get 'attributes disc))
                                      :shareUrl (alist-get 'shareUrl (alist-get 'attributes disc))))
                                   discussions)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error fetching discussions: %s" error-thrown)))))

(defun flarum-refresh ()
  "Refresh the discussions list."
  (interactive)
  (flarum--fetch-discussions
   (lambda (discussions)
     (setq flarum-discs discussions)
     (when (derived-mode-p 'flarum-mode)
       (flarum-draw-buffer)))))

;;; Entry Point

;;;###autoload
(defun flarum ()
  "Browse Flarum discussions.
\\{flarum-mode-map}"
  (interactive)
  (switch-to-buffer "*Flarum*")
  (unless (derived-mode-p 'flarum-mode)
    (flarum-mode))
  (flarum-refresh))

(provide 'flarum)
;;; flarum.el ends here
