;;; gmail-helper.el --- Talking to the Python helper.

;; Copyright (c) 2015 Chris Done. All rights reserved.

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

(defun gmail-helper-run (json)
  "Run the Python helper with the given arguments."
  (with-temp-buffer
    (call-process-region (point-min)
                         (point-max)
                         "python"
                         nil
                         (current-buffer)
                         nil
                         "gmail.py"
                         (format "%S" json))
    (buffer-string)))

(defun gmail-helper-labels-list ()
  "Get a list of labels."
  (gmail-helper-run (list "labels" "list")))

(defun gmail-helper-threads-list (tags query)
  "Get a list of threads that match the given TAGS and QUERY."
  (gmail-helper-run (list "threads" "list" tags query)))

(defun gmail-helper-threads-get (id format)
  "Retrieve the thread of messages by ID with FORMAT level of detail:
   full
   metadata
   minimal"
  (gmail-helper-run (list "threads" "get" id (symbol-name format))))

(defun gmail-helper-messages-list (tags query)
  "Get a list of messages that match the given TAGS and QUERY."
  (gmail-helper-run (list "messages" "list" tags query)))

(defun gmail-helper-messages-get (id format)
  "Retrieve the message of messages by ID with FORMAT level of detail:
   full
   metadata
   minimal"
  (gmail-helper-run (list "messages" "get" id (symbol-name format))))

(defun gmail-helper-drafts-list ()
  "Get a list of drafts."
  (gmail-helper-run (list "drafts" "list")))

(defun gmail-helper-profile-get ()
  "Get the user's profile."
  (gmail-helper-run (list "profile" "get")))

(provide 'gmail-helper)
