;;; gmail-cache.el --- For caching email data.

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

(defun gmail-cache-ensure ()
  "Ensure the cache is setup properly."
  (unless (file-exists-p (gmail-cache-dir))
    (make-directory (gmail-cache-dir))))

(defun gmail-cache-dir ()
  "The cache dir to write cache files to."
  (concat temporary-file-directory "gmail-cache"))

(defmacro with-gmail-caching (key &rest body)
  (let ((existing (gensym "existing"))
        (result (gensym "result")))
    `(let ((,existing (gmail-cache-get ,key)))
       (if ,existing
           ,existing
         (let ((,result (progn ,@body)))
           (gmail-cache-put ,key ,result))))))

(defun gmail-cache-put (key value)
  "Write to the cache."
  (gmail-cache-ensure)
  (with-temp-buffer
    (insert (format "%S" value))
    (write-file (gmail-cache-key-file key)))
  value)

(defun gmail-cache-key-file (key)
  "Make a filename for the given key."
  (concat (gmail-cache-dir) "/" key ".json"))

(defun gmail-cache-get (key)
  "Read from the cache."
  (gmail-cache-ensure)
  (when (file-exists-p (gmail-cache-key-file key))
    (with-temp-buffer
    (insert-file-contents (gmail-cache-key-file key))
    (read (buffer-string)))))

(defun gmail-cache-p (key)
  "Check to see if something exists in the cache."
  (gmail-cache-ensure)
  (file-exists-p (gmail-cache-key-file key)))

(defun gmail-cache-delete (key)
  "Invalidate the cache."
  (gmail-cache-ensure)
  (when (gmail-cache-p key)
    (delete-file (gmail-cache-key-file key))))

(provide 'gmail-cache)
