;;; configure.el --- Configure emacs for use in Makefile with --batch.

;;; Code:
(add-to-list 'load-path (expand-file-name "site-lisp/org-8.3.6/lisp"
                                          user-emacs-directory))

(require 'org)

(message "Using Org version: %s" (org-version))
