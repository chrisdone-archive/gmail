;;; gmail-search-mode.el --- Search for GMail threads.

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

(require 'gmail)
(require 'gmail-headers)
(require 'gmail-encoding)

(defcustom gmail-search-mode-page-columns
  80
  "Width of the page."
  :type 'integer
  :group 'gmail)

(defcustom gmail-search-mode-date-length
  13
  "Length of the date string."
  :type 'integer
  :group 'gmail)

(defcustom gmail-search-mode-date-format
  "%H:%M, %d %b %Y"
  "Format string for dates."
  :type 'string
  :group 'gmail)

(defface gmail-search-mode-from-face
  '((((class color)) :inherit zenburn-blue-2))
  "Face for the sender of emails."
  :group 'gmail)

(defface gmail-search-mode-subject-face
  '((((class color)) :inherit zenburn-blue))
  "Face for subjects."
  :group 'gmail)

(defface gmail-search-mode-subject-unread-face
  `((((class color)) :foreground ,zenburn-cyan))
  "Face for unread subjects."
  :group 'gmail)

(defface gmail-search-mode-read-face
  `((((class color))))
  "Face for read email.s"
  :group 'gmail)

(defface gmail-search-mode-unread-face
  `((((class color)) :weight bold))
  "Face for unread emails."
  :group 'gmail)

(defface gmail-search-mode-snippet-face
  '((((class color)) :inherit zenburn-green+1))
  "Face for the snippet."
  :group 'gmail)

(defface gmail-search-mode-date-face
  '((((class color)) :inherit zenburn-orange))
  "Face for the date."
  :group 'gmail)

(defface gmail-search-mode-labels-face
  '((((class color)) :background "#393939" :foreground "#999"))
  "Face for labels."
  :group 'gmail)

(defface gmail-search-mode-query-face
  '((((class color)) :foreground "#999"))
  "Face for queries/"
  :group 'gmail)

(defvar gmail-search-mode-query ""
  "Current query being used.")
(make-variable-buffer-local 'gmail-search-mode-query)

(defvar gmail-search-mode-labels '()
  "Current labels being used.")
(make-variable-buffer-local 'gmail-search-mode-labels)

(defvar gmail-search-mode-threads '()
  "Current thread results.")
(make-variable-buffer-local 'gmail-search-mode-threads)

(define-derived-mode gmail-search-mode special-mode "GMail-Search"
  "Search GMail for your emails."
  (gmail-search-mode-revert))

(define-key gmail-search-mode-map (kbd "g") 'gmail-search-mode-revert)
(define-key gmail-search-mode-map (kbd "s") 'gmail-search-mode-search)

(defun gmail-search-mode-revert ()
  "Revert the current buffer; in other words: re-run the search."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Refreshing…" 'face 'font-lock-comment)))
  (redisplay t)
  (cond
   (t)
   ((and (null gmail-search-mode-labels)
         (string= "" gmail-search-mode-query))
    (setq gmail-search-mode-threads
          (gmail-helper-threads-list '("INBOX") "")))
   (t
    (setq gmail-search-mode-threads
          (gmail-helper-threads-list gmail-search-mode-labels
                                     gmail-search-mode-query))))
  (gmail-search-mode-render))

(defun gmail-search-mode-render ()
  "Render the threads list."
  (let ((original-point (point)))
    (let ((inhibit-read-only t)
          (thread-results gmail-search-mode-threads)
          (threads (plist-get gmail-search-mode-threads :threads)))
      (erase-buffer)
      (insert "Search: " (propertize gmail-search-mode-query 'face 'gmail-search-mode-query-face)
              "\n")
      (insert (format "%d results\n" (plist-get thread-results :resultSizeEstimate)))
      (insert "\n")
      (insert (propertize "Downloading messages…" 'face 'font-lock-comment))
      (redisplay t)
      (let ((ids (remove-if
                  (lambda (id)
                    (gmail-cache-p (concat "message-" id)))
                  (mapcar (lambda (thread-result)
                            (plist-get thread-result :id))
                          threads))))
        (unless (null ids)
          (gmail-helper-messages-get-many ids 'metadata)))
      (delete-region (line-beginning-position)
                     (line-end-position))
      (let ((meta-threads
             (mapcar
              (lambda (thread-result)
                (gmail-helper-messages-get (plist-get thread-result :id) 'metadata))
              threads)))
        (cl-loop for thread in meta-threads
                 do (gmail-search-mode-render-thread thread)))
      (goto-char original-point))))

(defun gmail-search-mode-render-thread (thread)
  "Render the given thread."
  (let* ((snippet (gmail-encoding-decode-html (plist-get thread :snippet)))
         (payload (plist-get thread :payload))
         (headers (plist-get payload :headers))
         (subject (gmail-headers-lookup "Subject" headers))
         (from (gmail-headers-lookup "From" headers))
         (date (mail-header-parse-date (gmail-headers-lookup "Date" headers)))
         (labels (plist-get thread :labelIds))
         (unread (remove-if-not (lambda (label) (string= label "UNREAD"))
                                labels)))
    (let ((view
           (concat (gmail-search-mode-ellipsis
                    (propertize (concat from " ") 'face 'gmail-search-mode-from-face)
                    (- gmail-search-mode-page-columns (1+ gmail-search-mode-date-length)))
                   (propertize
                    (format-time-string gmail-search-mode-date-format date)
                    'face 'gmail-search-mode-date-face)
                   "\n"
                   (gmail-search-mode-ellipsis
                    (concat (gmail-search-mode-ellipsis
                             (propertize subject
                                         'face (if unread
                                                   'gmail-search-mode-subject-unread-face
                                                 'gmail-search-mode-subject-face))
                             (/ gmail-search-mode-page-columns 2))
                            (if (string= "" subject)
                                "" " ")
                            (propertize snippet
                                        'face 'gmail-search-mode-snippet-face))
                    (1- gmail-search-mode-page-columns))
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
          (point (point)))
      (insert view)
      (let ((o (make-overlay point (point))))
        (if unread
            (overlay-put o 'face 'gmail-search-mode-unread-face)
          (overlay-put o 'face 'gmail-search-mode-read-face))))))

(defun gmail-search-mode-ellipsis (string n)
  "Take from the string and add an ellipsis if it was cut off."
  (concat (substring string 0 (min (length string) n))
          (propertize
           (if (> (length string) n)
               "…"
             "")
           'face
           (get-text-property (max 0 (1- (length string))) 'face string))))

(defun gmail-search-mode-take (string n)
  "Take from the string and add an ellipsis if it was cut off."
  (substring string 0 (min (length string) n)))

(defun gmail-search-mode-set (labels query)
  "Set the query used for the search in the current buffer."
  (setq gmail-search-mode-query query)
  (setq gmail-search-mode-labels labels)
  (gmail-search-mode-revert))

(defun gmail-search-mode-search ()
  "Search in the current search buffer."
  (interactive)
  (let ((q (read-from-minibuffer
            "Search: "
            gmail-search-mode-query)))
    (gmail-search-mode-set '() q)))

(provide 'gmail-search-mode)
