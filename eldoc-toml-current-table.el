;;; eldoc-toml-current-table.el --- TOML table name at point for ElDoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ma'or Kadosh
;;
;; Author: Ma'or Kadosh <git@avocadosh.xyz>
;; URL: https://github.com/it-is-wednesday/eldoc-toml-current-table
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: data
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Display the table name the field at point belongs to. Intended to ease navigation of large
;;  tables with many fields, where its header might get out of screen.
;;
;;; Code:

(defun eldoc-toml-current-table--remove-comment (line)
  "Remove comment from end of LINE.
Also remove any space between the actual line's ending to the comment's beginning."
  (replace-regexp-in-string "\s*#.*" "" line))

(defun eldoc-toml-current-table--current-table-name ()
  "Find which table the current line is under.
For example, in this scenario, where X is the cursor:

[a]
X
[b]

returns [a]."
  (let ((initial-point (point))
        (current-line (thing-at-point 'line 'no-properties)))

    ;; Iterate from current line backwards until we hit a table line
    (while (not (string-match-p "^\s*\\[" current-line))
      (forward-line -1)
      (setq current-line (thing-at-point 'line 'no-properties)))

    (goto-char initial-point)

    ;; Trimming since TOML lines can begin with whitespace
    (string-trim (eldoc-toml-current-table--remove-comment current-line))))

(defun eldoc-toml-current-table--callback (callback &rest _more)
  "Document the table the value at point is in and pass it to CALLBACK.
Add this to `eldoc-documentation-functions'."
  (let ((table (eldoc-toml-current-table--current-table-name)))
    ;; make sure we got some table value, because some TOML documents begin with comments - in
    ;; which case we should just be quiet.
    (when table
      (funcall callback (concat "In table: " table)))))

;;;###autoload
(defun eldoc-toml-current-table ()
  "Add the hook to `eldoc-documentation-functions'."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'eldoc-toml-current-table--callback nil 'local))

(provide 'eldoc-toml-current-table)
;;; eldoc-toml-current-table.el ends here
