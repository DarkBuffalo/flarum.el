;;; flarum.el --- Flarum unofficial api -*- lexical-binding: t; -*-


;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1")(request))
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
;;

;;; Code:

(require 'request)
(require 'cl-lib)

;;; Constantes
(defconst flarum-api-endpoint
  "https://emacs.gnu.re/public/api")

;;; variables
(defvar flarum-discs '()
  "List of discussions displayed in the *flarum* buffer.")

;;; Faces
(defface flarum-title-face
  '((t :inherit font-lock-type-face))
  "Face used for the disc title.")

;;; Keys

(defvar flarum-mode-map
  (let ((map (make-sparse-keymap))
	(mappings '("o" flarum-open)))
    (cl-loop for (key fn) on mappings by #'cddr
	     do (define-key map (kbd key) fn))
    map)
  "Local keymap for `flarum-mode' buffers.")

(defun flarum-open ()
  "Open the discussion on web."
  (interactive)
  (browse-url (tabulated-list-get-id)))


(defun flarum--format-title (title)
  "Format the discussions TITLE in the *Flarum* buffer."
  (propertize title 'face `(:inherit flarum-title-face)))

(define-derived-mode flarum-mode tabulated-list-mode "flarum-mode"
  "Major mode for flarum.")

(cl-defstruct (flarum-disc (:constructor flarum--create-disc)
			   (:copier nil))
  "Metadata for a Flarum discussion."
  (id 0 :read-only t)
  (title "" :read-only t)
  (slug "" :read-only t)
  (shareUrl "" :read-only t))


(defun flarum-draw-buffer ()
  "Draw buffer with discussions entries."
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (read-only-mode 1)
  (setq tabulated-list-format `[("ID" 3 t)
				("Titre" ,(- (window-width) 24) :right-align t)])
  (setq tabulated-list-entries (mapcar #'flarum--insert-entry
				       flarum-discs))
  (use-local-map flarum-mode-map)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun flarum--insert-entry (data)
  "Insert DATA into the current buffer."
  (list (flarum-disc-shareUrl data)
	(vector (flarum-disc-id data)
		(flarum--format-title (flarum-disc-title data)))))


;;; API
(defun flarum--discussions-api ()
  "Call the Flarum discussions api."
  (let* ((url (concat flarum-api-endpoint "/discussions"))
	 (req (request
		url
		:parser 'json-read
		:sync 't ))
	 (data (alist-get 'data (request-response-data req))))

    (dotimes (i (length data))
      (let ((v (aref data i)))
	(aset data i
	      (flarum--create-disc
	       :id (assoc-default 'id v)
	       :title (assoc-default 'title (assoc-default 'attributes v))
	       :slug (assoc-default 'slug (assoc-default 'attributes v))
	       :shareUrl (assoc-default 'shareUrl (assoc-default 'attributes v))))))
    data))



;;;###autoload
(defun flarum ()
  "Flarum.
\\{flarum-mode-map}"
  (interactive)
  (switch-to-buffer "*Flarum*")
  (unless (eq major-mode 'flarum-mode)
    (flarum-mode)
    (call-interactively #'flarum-search)))


(provide 'flarum)
;;; flarum.el ends here
