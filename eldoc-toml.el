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

(require 'cl-lib)

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

Returns the table's name. If we aren't under any table (before the first table
of the file), returns nil."
  (let ((initial-point (point)))
    (defvar t/line)
    (defvar t/result)
    ;; Iterate from current line backwards until we hit a table line or until the first line
    (cl-loop (setq t/line (thing-at-point 'line 'no-properties))
             (when (string-match-p "^\s*\\[" t/line)
               ;; Trimming since TOML lines can begin with whitespace
               (setq t/result (string-trim (eldoc-toml--remove-comment t/line)))
               (cl-return))

             ;; Return nil if we reached the beginning of the buffer without any matching line
             (when (<= (line-number-at-pos) 1)
               (setq t/result nil)
               (cl-return))

             (forward-line -1))

    (goto-char initial-point)

    t/result))

(defun eldoc-toml--current-key-name ()
  "Return the current key name."
  (let ((initial-point (point)))
    (defvar k/line)
    (defvar k/result)
    ;; Iterate from current line backwards until we see key definition: xxx = ...
    (cl-loop (setq k/line (thing-at-point 'line 'no-properties))

             (when (string-match-p ".+\s*=" k/line)
               ;; Remove everything in the matching line except the key name
               (setq k/result (string-trim (replace-regexp-in-string "\s*=.*" "" k/line)))
               (cl-return))

             ;; Return nil if we reached the beginning of the buffer without any matching line
             (when (<= (line-number-at-pos) 1)
               (setq k/result nil)
               (cl-return))

             (forward-line -1))

    (goto-char initial-point)

    k/result))

(defun eldoc-toml--callback (callback &rest _more)
  "Document the table the value at point is in and pass it to CALLBACK.
Add this to `eldoc-documentation-functions'."
  (let* ((table (eldoc-toml--current-table-name))
         (key (eldoc-toml--current-key-name))
         (key-str (if key (concat "." key))))
    ;; make sure we got some table value, because some TOML documents begin with comments - in
    ;; which case we should just be quiet.
    (if table
        (funcall callback (concat table key-str)))))

;;;###autoload
(defun eldoc-toml ()
  "Add the hook to `eldoc-documentation-functions'."
  (interactive)
  (add-hook 'eldoc-documentation-functions #'eldoc-toml--callback nil 'local))

(provide 'eldoc-toml)
;;; eldoc-toml.el ends here
