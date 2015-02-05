;;; gmail-compose-mode.el --- Email writing mode for GMail

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-derived-mode gmail-compose-mode message-mode "GMail-Compose"
  "Email writing mode.")

(define-key gmail-compose-mode-map (kbd "C-c C-c") 'gmail-compose-mode-send)

(defun gmail-compose-mode-send ()
  (interactive)
  (save-excursion
    (let ((from (progn (goto-char (point-min))
                       (when (search-forward-regexp "^From: " nil t 1)
                         (buffer-substring-no-properties (point) (line-end-position)))))
          (to (progn (goto-char (point-min))
                     (when (search-forward-regexp "^To: " nil t 1)
                       (buffer-substring-no-properties (point) (line-end-position)))))
          (in-reply-to (progn (goto-char (point-min))
                              (when (search-forward-regexp "^In-Reply-To: " nil t 1)
                                (buffer-substring-no-properties (point) (line-end-position)))))
          (thread-id (progn (goto-char (point-min))
                            (when (search-forward-regexp "^ThreadId: " nil t 1)
                              (buffer-substring-no-properties (point) (line-end-position)))))
          (references (progn (goto-char (point-min))
                             (when (search-forward-regexp "^References: " nil t 1)
                               (buffer-substring-no-properties (point) (line-end-position)))))
          (subject (progn (goto-char (point-min))
                          (when (search-forward-regexp "^Subject: " nil t 1)
                            (buffer-substring-no-properties (point) (line-end-position)))))
          (body (progn (goto-char (point-min))
                       (search-forward-regexp "\n\n" nil t 1)
                       (buffer-substring-no-properties (point) (point-max)))))
      (gmail-helper-send from
                         to
                         subject
                         body
                         in-reply-to
                         references
                         thread-id)
      (gmail-search)
      (gmail-search-mode-set '("SENT") ""))))

(provide 'gmail-compose-mode)
