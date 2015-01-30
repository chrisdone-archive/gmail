;;; gmail-thread-mode.el --- Mode for viewing a GMail thread.

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

(defvar gmail-thread-mode-thread-id nil
  "Current thread id.")
(make-variable-buffer-local 'gmail-thread-mode-thread-id)

(defface gmail-thread-mode-read-face
  `((((class color)) :background "#393939"))
  "Face for read email.s"
  :group 'gmail)

(defface gmail-thread-mode-unread-face
  `((((class color)) :background "#393939" :weight bold))
  "Face for unread emails."
  :group 'gmail)

(define-derived-mode gmail-thread-mode special-mode "GMail-Thread"
  "View a GMail thread.")

(define-key gmail-thread-mode-map (kbd "g") 'gmail-thread-mode-revert)

(defun gmail-thread-mode-revert ()
  "Revert the current buffer; in other words: re-run the thread."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Refreshing…" 'face 'font-lock-comment)))
  (redisplay t)
  (gmail-thread-mode-render))

(defun gmail-thread-mode-render ()
  "Render the messages list."
  (let ((inhibit-read-only t)
        (thread (gmail-helper-threads-get gmail-thread-mode-thread-id 'full)))
    (erase-buffer)
    (let* ((message (car (plist-get thread :messages)))
           (payload (plist-get message :payload))
           (headers (plist-get payload :headers))
           (subject (gmail-headers-lookup "Subject" headers))
           (from (gmail-headers-lookup "From" headers))
           (labels (plist-get message :labelIds))
           (date (mail-header-parse-date (gmail-headers-lookup "Date" headers)))
           (unread (remove-if-not (lambda (label) (string= label "UNREAD"))
                                  labels)))
      (insert (propertize subject
                          'face (if unread
                                    'gmail-search-mode-subject-unread-face
                                  'gmail-search-mode-subject-face))
              " "
              (propertize
               (format-time-string gmail-search-mode-date-format date)
               'face 'gmail-search-mode-date-face)
              "\n"
              (mapconcat #'identity
                         (mapcar (lambda (label)
                                   (propertize label
                                               'face 'gmail-search-mode-labels-face))
                                 (mapcar #'gmail-labels-pretty labels))
                         (propertize ", "
                                     'face
                                     'gmail-search-mode-from-face))
              "\n\n"))
    (cl-loop for message in (plist-get thread :messages)
             do (gmail-thread-mode-render-message message))
    (goto-char (point-min))))

(defun gmail-thread-mode-render-message (message)
  "Render the message."
  (let* ((snippet (gmail-encoding-decode-html (plist-get message :snippet)))
         (payload (plist-get message :payload))
         (headers (plist-get payload :headers))
         (from (gmail-headers-lookup "From" headers))
         (date (mail-header-parse-date (gmail-headers-lookup "Date" headers)))
         (labels (plist-get message :labelIds))
         (unread (remove-if-not (lambda (label) (string= label "UNREAD"))
                                labels)))
    (let ((view
           (concat (gmail-search-mode-ellipsis
                    (propertize (concat from " ") 'face 'gmail-search-mode-from-face)
                    (- gmail-search-mode-page-columns (1+ gmail-search-mode-date-length)))
                   (propertize
                    (format-time-string gmail-search-mode-date-format date)
                    'face 'gmail-search-mode-date-face)
                   (if t
                       ""
                     (concat "\n"
                             (gmail-search-mode-ellipsis
                              (propertize snippet
                                          'face 'gmail-search-mode-snippet-face)
                              (1- gmail-search-mode-page-columns))))
                   "\n"))
          (point (point)))
      (insert view)
      (let ((o (make-overlay point (point))))
        (if unread
            (overlay-put o 'face 'gmail-thread-mode-unread-face)
          (overlay-put o 'face 'gmail-thread-mode-read-face)))
      (insert "\n")
      (let ((start (point)))
        (insert (propertize (gmail-thread-mode-plaintext payload)
                            'face 'gmail-search-mode-body-face)
                "\n\n")
        (decode-coding-region start (point)
                              'utf-8)
        (gmail-thread-mode-fill-lines start (point))))))

(defun gmail-thread-mode-plaintext (payload)
  "Get the plain text from a message payload."
  (if (string-prefix-p "text/plain" (plist-get payload :mimeType))
      (gmail-encoding-decode-base64
       (plist-get (plist-get payload :body) :data))
    (let ((plain (car (remove-if-not (lambda (part)
                                       (string-prefix-p "text/plain" (plist-get part :mimeType)))
                                     (plist-get payload :parts)))))
      (if plain
          (gmail-encoding-decode-base64
           (plist-get (plist-get plain :body) :data))
        (let ((multipart (car (remove-if-not (lambda (part)
                                               (string-prefix-p "multipart/alternative" (plist-get part :mimeType)))
                                             (plist-get payload :parts)))))
          (if multipart
              (let ((plain (car (remove-if-not (lambda (part)
                                                 (string-prefix-p "text/plain" (plist-get part :mimeType)))
                                               (plist-get multipart :parts)))))
                (if plain
                    (gmail-encoding-decode-base64 (plist-get (plist-get plain :body) :data))
                  (progn (message "Unable to find a plain/text field from this email, even in multi-parts: %S" payload)
                         "")))
            (progn (message "Unable to find a plain/text field from this email: %S" payload)
                   "")))))
    ))

(defun gmail-thread-mode-fill-lines (start end)
  "Fill lines longer than 80 columns, to make it more readable."
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" end t 1)
      (forward-char -1)
      (when (> (current-column) 80)
        (fill-region (line-beginning-position)
                     (line-end-position)))
      (goto-char (line-end-position))
      (forward-char 1))))

(provide 'gmail-thread-mode)
