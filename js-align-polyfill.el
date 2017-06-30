;;;; js-align-polyfill.el --- Polyfill for js-align -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;; Maintainer: Daniel Colascione <dan.colascione@gmail.com>
;; Version: 9
;; Date: 2009-07-25
;; Keywords: languages, javascript

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; This is code copied from the latest js.el and will be removed once
;; it has moved to the release version of Emacs.

;;; Code:

(defconst --js--syntax-propertize-regexp-regexp
  (rx
   ;; Start of regexp.
   "/"
   (0+ (or
        ;; Match characters outside of a character class.
        (not (any ?\[ ?/ ?\\))
        ;; Match backslash quoted characters.
        (and "\\" not-newline)
        ;; Match character class.
        (and
         "["
         (0+ (or
              (not (any ?\] ?\\))
              (and "\\" not-newline)))
         "]")))
   (group (zero-or-one "/")))
  "Regular expression matching a JavaScript regexp literal.")

(defun --js-syntax-propertize-regexp (end)
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      ;; A /.../ regexp.
      (goto-char (nth 8 ppss))
      (when (looking-at --js--syntax-propertize-regexp-regexp)
        ;; Don't touch text after END.
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defun --js-syntax-propertize (start end)
  ;; JavaScript allows immediate regular expression objects, written /.../.
  (goto-char start)
  (--js-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
    ;; FIXME: Allow regexps after infix ops like + ...
    ;; https://developer.mozilla.org/en/JavaScript/Reference/Operators
    ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
    ;; point I think only * and / would be missing which could also be added,
    ;; but need care to avoid affecting the // and */ comment markers.
    ;; johnhooks - added '[' and '=>'
    ("\\(?:^\\|[\[=([{,:;|&!]\\|\\_<return\\_>\\|=>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
     (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   ;; If the / is at the beginning of line, we have to check
                   ;; the end of the previous text.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (--js-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b")))
   (point) end))

(provide 'js-align-polyfill)

;;; js-align-polyfill.el ends here
