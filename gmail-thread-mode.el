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
  `()
  "Face for read thread headers."
  :group 'gmail)

(defface gmail-thread-mode-unread-face
  `()
  "Face for unread emails."
  :group 'gmail)

(define-derived-mode gmail-thread-mode special-mode "GMail-Thread"
  "View a GMail thread.")

(define-key gmail-thread-mode-map (kbd "g") 'gmail-thread-mode-revert)
(define-key gmail-thread-mode-map (kbd "l") 'gmail-thread-mode-labels)
(define-key gmail-thread-mode-map (kbd "a") 'gmail-thread-mode-archive)
(define-key gmail-thread-mode-map (kbd "r") 'gmail-thread-mode-mark-as-read)
(define-key gmail-thread-mode-map (kbd "d") 'gmail-thread-mode-delete)
(define-key gmail-thread-mode-map (kbd "n") 'gmail-thread-mode-next)
(define-key gmail-thread-mode-map (kbd "p") 'gmail-thread-mode-prev)
(define-key gmail-thread-mode-map (kbd "c") 'gmail-thread-mode-reply)

(defun gmail-thread-mode-reply ()
  (interactive)
  (let* ((thread-messages (gmail-helper-threads-get gmail-thread-mode-thread-id 'full))
         (thread (car (plist-get thread-messages :messages)))
         (thread-payload (plist-get thread :payload))
         (thread-headers (plist-get thread-payload :headers))
         (thread-message-id (gmail-headers-lookup "Message-Id" thread-headers))

         (subject (gmail-headers-lookup "Subject" thread-headers))
         (message-id (get-text-property (point) 'gmail-thread-mode-message-id))
         (message (car (remove-if-not (lambda (message)
                                        (string= message-id (plist-get message :id)))
                                      (plist-get thread-messages :messages))))
         (message-payload (plist-get message :payload))
         (message-headers (plist-get message-payload :headers))
         (from (gmail-headers-lookup "From" message-headers))
         (message-message-id (gmail-headers-lookup "Message-Id" message-headers)))
    (switch-to-buffer (get-buffer-create "*gmail-compose*"))
    (unless (eq major-mode 'gmail-compose-mode)
      (gmail-compose-mode))
    (delete-region (point-min) (point-max))
    (insert "References: " thread-message-id "\n")
    (insert "In-Reply-To: " (or message-message-id thread-message-id) "\n")
    (insert "Subject: " subject "\n")
    (insert "To: " from "\n")
    (insert "From: Christopher Done <chrisdone@gmail.com>\n")
    (insert "\n")))

(defun gmail-thread-mode-next ()
  (interactive)
  (let ((os (sort (remove-if-not (lambda (o) (overlay-get o 'gmail-thread-mode-message-header))
                                 (overlays-in (point) (point-max)))
                  (lambda (a b)
                    (< (overlay-start a)
                       (overlay-start b))))))
    (when os
      (if (= (overlay-start (car os))
             (point))
          (when (cadr os)
            (goto-char (overlay-start (cadr os))))
        (goto-char (overlay-start (car os))))
      (recenter-top-bottom 0))))

(defun gmail-thread-mode-prev ()
  (interactive)
  (let ((os (sort (remove-if-not (lambda (o) (overlay-get o 'gmail-thread-mode-message-header))
                                 (overlays-in (point-min) (point)))
                  (lambda (a b)
                    (> (overlay-start a)
                       (overlay-start b))))))
    (when os
      (if (= (overlay-start (car os))
             (point))
          (when (cadr os)
            (goto-char (overlay-start (cadr os))))
        (goto-char (overlay-start (car os))))
      (recenter-top-bottom 0))))

(defun gmail-thread-mode-delete ()
  (interactive)
  (let* ((message-id (get-text-property (point) 'gmail-thread-mode-message-id))
         (thread (gmail-helper-threads-get gmail-thread-mode-thread-id 'full))
         (message (car (remove-if-not (lambda (message)
                                        (string= message-id (plist-get message :id)))
                                      (plist-get thread :messages))))
         (labels (plist-get message :labelIds)))
    (unless message
      (error "No message at point."))
    (gmail-thread-mode-add-message-tag message-id "TRASH" t)
    (kill-buffer)))

(defun gmail-thread-mode-archive ()
  (interactive)
  (gmail-thread-mode-remove-thread-tag "INBOX" t)
  (kill-buffer))

(defun gmail-thread-mode-mark-as-read ()
  (interactive)
  (let* ((message-id (get-text-property (point) 'gmail-thread-mode-message-id))
         (thread (gmail-helper-threads-get gmail-thread-mode-thread-id 'full))
         (message (car (remove-if-not (lambda (message)
                                        (string= message-id (plist-get message :id)))
                                      (plist-get thread :messages))))
         (labels (plist-get message :labelIds)))
    (unless message
      (error "No message at point."))
    (gmail-thread-mode-remove-message-tag message-id "UNREAD" t)))

(defun gmail-thread-mode-labels ()
  "Adjust the labels."
  (interactive)
  (let* ((message-id (get-text-property (point) 'gmail-thread-mode-message-id))
         (thread (gmail-helper-threads-get gmail-thread-mode-thread-id 'full))
         (message (car (remove-if-not (lambda (message)
                                        (string= message-id (plist-get message :id)))
                                      (plist-get thread :messages))))
         (labels (plist-get message :labelIds)))
    (unless message
      (error "No message at point."))
    (let ((key
           (key-description
            (vector
             (read-key
              (concat (propertize "Label command: " 'face 'minibuffer-prompt)
                      (propertize "a: " 'face 'minibuffer-prompt)
                      "add, "
                      (propertize "r: " 'face 'minibuffer-prompt)
                      "remove"))))))
      (cond
       ((string= key "a")
        (let* ((chosen-label (ido-completing-read
                              "Label to add: "
                              (mapcar (lambda (l) (concat (plist-get l :id) " (" (plist-get l :name) ")"))
                                      gmail-labels)))
               (label-id (car (split-string chosen-label " "))))
          (gmail-thread-mode-add-message-tag message-id label-id)))
       ((string= key "r")
        (let* ((chosen-label (ido-completing-read
                              "Label to remove: "
                              (mapcar (lambda (l) (concat l " (" (gmail-labels-pretty l) ")"))
                                      labels)))
               (label-id (car (split-string chosen-label " "))))
          (gmail-thread-mode-remove-message-tag message-id label-id)))))))

(defun gmail-thread-mode-remove-thread-tag (label-id &optional no-refresh)
  (gmail-helper-threads-modify gmail-thread-mode-thread-id (list) (list label-id))
  (if no-refresh
      (gmail-cache-delete (format "thread-%s-%S" gmail-thread-mode-thread-id 'full))
    (gmail-thread-mode-revert))
  (message "Tag removed on whole thread."))

(defun gmail-thread-mode-remove-message-tag (message-id label-id &optional no-refresh)
  (gmail-helper-messages-modify message-id (list) (list label-id))
  (if no-refresh
      (gmail-cache-delete (format "thread-%s-%S" gmail-thread-mode-thread-id 'full))
    (gmail-thread-mode-revert))
  (message "Tag removed from message."))

(defun gmail-thread-mode-add-message-tag (message-id label-id &optional no-refresh)
  (gmail-helper-messages-modify message-id (list label-id) (list))
  (if no-refresh
      (gmail-cache-delete (format "thread-%s-%S" gmail-thread-mode-thread-id 'full))
    (gmail-thread-mode-revert))
  (message "Tag added to message."))

(defun gmail-thread-mode-revert ()
  "Revert the current buffer; in other words: re-run the thread."
  (interactive)
  (gmail-cache-delete (format "thread-%s-%S" gmail-thread-mode-thread-id 'full))
  (gmail-thread-mode-render))

(defun gmail-thread-mode-render ()
  "Render the messages list."
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (insert (propertize "Refreshing thread…" 'face 'font-lock-comment)))
  (redisplay t)
  (goto-char (point-min))
  (let ((inhibit-read-only t)
        (thread (gmail-helper-threads-get gmail-thread-mode-thread-id 'full))
        (final-marker (point-marker)))
    (erase-buffer)
    (let* ((message (car (plist-get thread :messages)))
           (payload (plist-get message :payload))
           (headers (plist-get payload :headers))
           (subject (gmail-headers-lookup "Subject" headers))
           (from (gmail-headers-lookup "From" headers))
           (to (gmail-headers-lookup "To" headers))
           (labels (plist-get message :labelIds))
           (date (mail-header-parse-date (gmail-headers-lookup "Date" headers)))
           (unread (remove-if-not (lambda (label) (string= label "UNREAD"))
                                  labels))
           (o (make-overlay (point-min)
                            (point-min))))
      (overlay-put o 'gmail-thread-mode-message-header t)
      (let ((header
             (concat (propertize subject
                                 'face (if unread
                                           'gmail-search-mode-subject-unread-face
                                         'gmail-search-mode-subject-face))
                     (if (> (length subject) gmail-search-mode-date-length)
                         "\n"
                       " ")
                     (propertize
                      (format-time-string gmail-search-mode-date-format date)
                      'face 'gmail-search-mode-date-face)
                     "\n"
                     "To: "
                     (propertize to
                                 'face
                                 'gmail-search-mode-from-face)
                     "\n"
                     (mapconcat #'identity
                                (mapcar (lambda (label)
                                          (propertize label
                                                      'face 'gmail-search-mode-labels-face))
                                        (mapcar #'gmail-labels-pretty labels))
                                (propertize ", "
                                            'face
                                            'gmail-search-mode-from-face))
                     "\n\n")))
        (insert (propertize header 'gmail-thread-mode-message-id (plist-get message :id)))))
    (cl-loop for message in (plist-get thread :messages)
             do (gmail-thread-mode-render-message message final-marker))
    (goto-char final-marker)))

(defun gmail-thread-mode-render-message (message final-marker)
  "Render the message."
  (let* ((snippet (gmail-encoding-decode-html (plist-get message :snippet)))
         (payload (plist-get message :payload))
         (headers (plist-get payload :headers))
         (from (gmail-headers-lookup "From" headers))
         (date (mail-header-parse-date (gmail-headers-lookup "Date" headers)))
         (labels (plist-get message :labelIds))
         (unread (remove-if-not (lambda (label) (string= label "UNREAD"))
                                labels)))
    (when (and unread (= final-marker (point-min)))
      (set-marker final-marker (point)))
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
                   "\n"
                   (mapconcat #'identity
                              (mapcar (lambda (label)
                                        (propertize label
                                                    'face 'gmail-search-mode-labels-face))
                                      (mapcar #'gmail-labels-pretty labels))
                              (propertize ", "
                                          'face
                                          'gmail-search-mode-from-face))
                   "\n"))
          (point (point)))
      (insert (propertize view 'gmail-thread-mode-message-id (plist-get message :id)))
      (let ((o (make-overlay point (point))))
        (overlay-put o 'gmail-thread-mode-message-header t)
        (if unread
            (overlay-put o 'face 'gmail-thread-mode-unread-face)
          (overlay-put o 'face 'gmail-thread-mode-read-face)))
      (gmail-thread-mode-attachments payload)
      (insert "\n")
      (let ((start (point)))
        (insert (propertize (gmail-thread-mode-plaintext payload)
                            'face 'gmail-search-mode-body-face)
                "\n\n")
        (decode-coding-region start (point)
                              'utf-8)
        (gmail-thread-mode-fill-lines start (point))))))

(defun gmail-thread-mode-attachments (payload)
  "List out the attachments of the email."
  (if (plist-get payload :parts)
      (mapconcat #'gmail-thread-mode-attachment
                 (plist-get payload :parts)
                 "")
    (gmail-thread-mode-attachment payload)))

(defun gmail-thread-mode-attachment (part)
  (let ((mime-type (plist-get part :mimeType)))
    (cond
     ((string-prefix-p "multipart/" mime-type)
      (gmail-thread-mode-attachments part))
     ((not (string= "" (plist-get part :filename)))
      (insert "Attachment: "
              (plist-get part :filename)
              " ("
              (file-size-human-readable (plist-get (plist-get part :body) :size) t)
              ")"
              "\n")))))

(defun gmail-thread-mode-plaintext-part (part)
  (let ((mime-type (plist-get part :mimeType)))
    (cond
     ((string-prefix-p "text/plain" mime-type)
      (gmail-encoding-decode-base64
       (plist-get (plist-get part :body) :data)))
     ((string-prefix-p "multipart/" mime-type)
      (gmail-thread-mode-plaintext part))
     ((string-prefix-p "text/html" mime-type)
      "")
     (t ""))))

(defun gmail-thread-mode-plaintext (payload)
  "Get the plain text from a message payload."
  (if (plist-get payload :parts)
      (mapconcat #'gmail-thread-mode-plaintext-part
                 (plist-get payload :parts)
                 "")
    (gmail-thread-mode-plaintext-part payload)))

(defun gmail-thread-mode-plaintext-part (part)
  (let ((mime-type (plist-get part :mimeType)))
    (cond
     ((string-prefix-p "text/plain" mime-type)
      (gmail-encoding-decode-base64
       (plist-get (plist-get part :body) :data)))
     ((string-prefix-p "multipart/" mime-type)
      (gmail-thread-mode-plaintext part))
     ((string-prefix-p "text/html" mime-type)
      "")
     (t ""))))

(defun gmail-thread-mode-fill-lines (start end)
  "Fill lines longer than 80 columns, to make it more readable."
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" nil t 1)
      (forward-char -1)
      (when (> (current-column) 80)
        (fill-region (line-beginning-position)
                     (line-end-position)))
      (goto-char (line-end-position))
      (forward-char 1))))

(provide 'gmail-thread-mode)
