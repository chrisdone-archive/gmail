;;; gmail-labels.el ---

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

(defvar gmail-labels '()
  "Current labels.")

(defun gmail-labels-update ()
  "Update the known labels."
  (interactive)
  (setq gmail-labels (plist-get (gmail-helper-labels-list) :labels)))

(defun gmail-labels-pretty (label)
  "Print a label in a nice way."
  (cond
   ((string= label "INBOX")
    "Inbox")
   ((string= label "CATEGORY_PERSONAL")
    "Private")
   ((string= label "UNREAD")
    "Unread")
   ((string= label "IMPORTANT")
    "⚑")
   ((string= label "CATEGORY_FORUMS")
    "Public")
   ((string= label "CATEGORY_PROMOTIONS")
    "Promotion/spam")
   ((string= label "CATEGORY_UPDATES")
    "Notification")
   ((string= label "TRASH")
    "Deleted")
   ((string= label "SENT")
    "Sent")
   ((string= label "STARRED")
    "★")
   (t (let ((found (car (remove-if-not (lambda (candidate)
                                         (string= (plist-get candidate :id)
                                                  label))
                                       gmail-labels))))
        (if found
            (plist-get found :name)
          label)))))

(provide 'gmail-labels)
