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

(defun gmail-helper-run (cmd)
  "Run the Python helper with the given commands."
  (let ((out (with-temp-buffer
               (call-process-region (point-min)
                                    (point-max)
                                    "python"
                                    nil
                                    (current-buffer)
                                    nil
                                    "gmail.py"
                                    (format "%S" (list cmd)))
               (buffer-string))))
    (with-current-buffer "*scratch*"
      (insert (format "%S =>" cmd)
              "\n"
              out
              "\n\n"))
    (car (read out))))

(defun gmail-helper-run-many (cmds)
  "Run the Python helper with the given commands."
  (let ((out (with-temp-buffer
               (call-process-region (point-min)
                                    (point-max)
                                    "python"
                                    nil
                                    (current-buffer)
                                    nil
                                    "gmail.py"
                                    (format "%S" cmds))
               (buffer-string))))
    (with-current-buffer "*scratch*"
      (insert (format "%S =>" cmds)
              "\n"
              out
              "\n\n"))
    (read out)))

(defun gmail-helper-labels-list ()
  "Get a list of labels."
  (with-gmail-caching
   "labels"
   (gmail-helper-run (list "labels" "list"))))

(defun gmail-helper-threads-list (tags query)
  "Get a list of threads that match the given TAGS and QUERY."
  (gmail-helper-run (list "threads" "list" tags query)))

(defun gmail-helper-threads-get (id format)
  "Retrieve the thread of messages by ID with FORMAT level of detail:
   full
   metadata
   minimal"
  (with-gmail-caching
   (format "thread-%s-%S" id format)
   (gmail-helper-run (list "threads" "get" id (symbol-name format)))))

(defun gmail-helper-threads-get-many (ids format)
  "Retrieve the threads by IDS with FORMAT level of detail:
   full
   metadata
   minimal"
  (let ((threads (gmail-helper-run-many
                   (mapcar (lambda (id)
                             (list "threads" "get" id (symbol-name format)))
                           ids))))
    (cl-loop for thread in threads
             collect (gmail-cache-put (concat "thread-" (plist-get thread :id))
                                      thread))))

(defun gmail-helper-drafts-list ()
  "Get a list of drafts."
  (gmail-helper-run (list "drafts" "list")))

(defun gmail-helper-profile-get ()
  "Get the user's profile."
  (gmail-helper-run (list "profile" "get")))

(provide 'gmail-helper)
