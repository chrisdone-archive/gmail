;;; gmail-headers.el ---

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

(defun gmail-headers-lookup (key assoc)
  (plist-get
   (find-if
    (lambda (header)
      (eq t
          (compare-strings key nil nil
                           (plist-get header :name)
                           nil nil
                           t)))
    assoc)
   :value))

(provide 'gmail-headers)
