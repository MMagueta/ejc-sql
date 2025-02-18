;;; ejc-corfu.el -- SQL completitions at point by corfu-mode (the part of ejc-sql).

;;; Copyright © 2020 - Kostafey <kostafey@gmail.com>
;;; Modified by Magueta <marcosmagueta@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'corfu)
(require 'ejc-completion-common)

(defun ejc-corfu-make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun ejc-corfu-add-meta (meta candidates)
  (-map (lambda (k) (list k meta))
        candidates))

(defun ejc-corfu-candidates (prefix)
  (let* ((prefix-1 (ejc-get-prefix-word))
         (prefix-2 (save-excursion
                     (search-backward "." nil t)
                     (ejc-get-prefix-word)))
         (res))
    (dolist (item
             (cl-remove-if-not
              (lambda (c) (string-prefix-p prefix (car c) t))
              (append
               (ejc-append-without-duplicates
                (ejc-corfu-add-meta
                 "ansi sql" (ejc-get-ansi-sql-words))
                (ejc-corfu-add-meta
                 "keyword" (ejc-get-keywords))
                'car :right)
               (ejc-corfu-add-meta
                "owner" (ejc-owners-candidates))
               (ejc-corfu-add-meta
                "table" (ejc-tables-candidates))
               (ejc-corfu-add-meta
                "view" (ejc-views-candidates))
               (if (not prefix-1)
                   (ejc-corfu-add-meta
                    "package" (ejc-packages-candidates)))
               (ejc-corfu-add-meta
                "column" (ejc-colomns-candidates)))))
      (push (ejc-corfu-make-candidate item) res))
    res))

(defun ejc-corfu-annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun ejc-corfu-doc-buffer (candidate)
  (corfu--make-buffer (ac-ejc-documentation candidate)))

(defun ejc-corfu-completion-at-point ()
  "Completion-at-point function for ejc-corfu."
  (when (bound-and-true-p ejc-sql-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds)
            (cdr bounds)
            (ejc-corfu-candidates (thing-at-point 'symbol))
            :annotation-function #'ejc-corfu-annotation
            :company-doc-buffer #'ejc-corfu-doc-buffer))))

(provide 'ejc-corfu)

;;; ejc-corfu.el ends here
