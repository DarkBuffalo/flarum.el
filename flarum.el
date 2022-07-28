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
(require 'cl-lib)


(defvar flarum-discs '()
  "List of videos displayed in the *peertube* buffer.")

(defface flarum-title-face
  '((t :inherit font-lock-type-face))
  "Face used for the disc title.")

(defun flarum--format-title (title)
  "Format the video TITLE int the *peertube* buffer."
  (propertize title 'face `(:inherit flarum-title-face)))

(define-derived-mode flarum-mode tabulated-list-mode "flarum-mode"
  "Major mode for flarum.")

(defun flarum-draw-buffer ()
  "Draw buffer with discussions entries."
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (read-only-mode 1)
  (setq tabulated-list-format `[("Title" 50 t)])
  (setq tabulated-list-entries (mapcar #'flarum--insert-entry
				       peertube-videos))
  (tabulated-list-init-header)
  (tabulated-list-print))



(cl-defstruct (flarum-disc (:constructor flarum--create-disc)
			   (:copier nil))
  "Metadata for a Flarum discussion."
  (id 0 :read-only t)
  (title "" :read-only t)
  (slug "" :read-only t)
  (shareUrl "" :read-only t))


(defun flarum--insert-entry (data)
  "Insert VIDEO into the current buffer."
  (list (flarum-discs-shareUrl data)
	(vector (flarum--format-title (flarum-disc-title video))
		)))



(defun flarum--discussions-api ()
  "Call the Flarum discussions api."
  (let* ((url "https://emacs.gnu.re/public/api/discussions")
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

(defun flarum-search ()
  (interactive)
  (let ((data (flarum--discussions-api)))
    (setq flarum-discs data)
    (flarum-draw-buffer)))


(defun flarum ()
  "Flarum."
  (interactive)
  (switch-to-buffer "*Flarum*")
  (unless (eq major-mode 'flarum-mode)
    (flarum-mode)
    (call-interactively #'flarum-search)))


(provide 'flarum)
;;; flarum.el ends here
