;;; eldoc-toml-current-table.el --- TOML table name at point for ElDoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ma'or Kadosh
;;
;; Author: Ma'or Kadosh <maor@avocadosh.xyz>
;; URL: https://github.com/it-is-wednesday/eldoc-toml-current-table
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (dash "2.17"))
;; Keywords: toml, eldoc
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Display the table name the field at point belongs to. Intended to ease navigation of large
;;  tables with many fields, where its header might get out of screen.
;;
;;; Code:

(require 'dash)

(defun eldoc-toml-current-table--remove-comment (line)
  "Remove comment from end of LINE.
Also remove any space between the actual line's ending to the comment's beginning."
  (replace-regexp-in-string "\s*#.*" "" line))

(defun eldoc-toml-current-table--callback (callback &rest _more)
  "Document the table the value at point is in and pass it to CALLBACK..
Add it to `eldoc-documentation-functions'."
  (let* ((lines (split-string (buffer-string) "\n"))
         (table (-some->> lines
                  (-take (line-number-at-pos))
                  reverse
                  (--first (string-match-p "^\s*\\[" it))
                  eldoc-toml-current-table--remove-comment
                  string-trim-left)))
    ;; make sure we got some table value, because some TOML documents begin with comments - in
    ;; which case we should just be quiet.
    (when table
      ;; Trimming since TOML lines can begin with whitespace
      (funcall callback (concat "In table: " table)))))

(defun eldoc-toml-current-table-setup-eldoc ()
  "Add the hook to `eldoc-documentation-functions'."
  (add-hook 'eldoc-documentation-functions #'eldoc-toml-current-table--callback nil t))


(provide 'eldoc-toml-current-table-setup-eldoc)
;;; eldoc-toml-current-table.el ends here
