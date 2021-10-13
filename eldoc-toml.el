;;; eldoc-toml.el --- TOML table name at point for ElDoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ma'or Kadosh
;;
;; Author: Ma'or Kadosh <git@avocadosh.xyz>
;; URL: https://github.com/it-is-wednesday/eldoc-toml
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

(defun eldoc-toml--remove-comment (line)
  "Remove comment from end of LINE.
Also remove any space between the actual line's ending to the comment's beginning."
  (replace-regexp-in-string "\s*#.*" "" line))

(defun eldoc-toml--current-table-name ()
  "Find which table the current line is under.
For example, in this scenario, where X is the cursor:

[a]
X
[b]

returns [a].

Returns the string to be displayed by eldoc: 'In table: [tablename]'.
If we aren't under any table (before the first table of the file), returns nil."
  (let ((initial-point (point))
        (line (thing-at-point 'line 'no-properties)))

    ;; Iterate from current line backwards until we hit a table line or until the first line
    (while (and line (not (string-match-p "^\s*\\[" line)))
      (forward-line -1)

      (setq line (thing-at-point 'line 'no-properties))

      ;; Set line to nil if we reached the beginning of the buffer without any matching line
      (when (<= (line-number-at-pos) 1)
        (setq line nil)))

    (goto-char initial-point)

    (if line
        ;; Trimming since TOML lines can begin with whitespace
        (concat "In table: " (string-trim (eldoc-toml--remove-comment line)))
      nil)))

(defun eldoc-toml--callback (callback &rest _more)
  "Document the table the value at point is in and pass it to CALLBACK.
Add this to `eldoc-documentation-functions'."
  (let ((table (eldoc-toml--current-table-name)))
    ;; make sure we got some table value, because some TOML documents begin with comments - in
    ;; which case we should just be quiet.
    (when table
      (funcall callback table))))

;;;###autoload
(defun eldoc-toml ()
  "Add the hook to `eldoc-documentation-functions'."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'eldoc-toml--callback nil 'local))

(provide 'eldoc-toml)
;;; eldoc-toml.el ends here
