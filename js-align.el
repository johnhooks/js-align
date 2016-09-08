
;;; js-align.el --- Manage the indentation of JavaScript.

;; Copyright (C) 2016 John Hooks
;; Copyright (C) 2008-2016 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;;         John Hooks <john@bitmachina.com>

;; Maintainer: John Hooks <john@bitmachina.com>
;; Version: 0.1
;; Keywords: languages, javascript

;; This file is part of js-align
;;
;; js-align is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; js-align is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with js-align.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is based on the indentation code of Daniel Calascione's
;; `js-mode' which is based on Karl Landstrom's `javascript-mode'.

;; This software is an attempt to improve the handling of JavaScript
;; indentation in a flexible and easy to reason about way.

;; General Remarks:
;;
;; Exported names start with "js-align"; private names start with
;; "js-align--".

;;; Code:

(require 'js)
(require 'cl-lib)

(defvar js-align--empty-re "\\s-*\\($\\|/[/*]\\|//\\)")

;; Based on `js--indent-operator-re' from `js.el'. Though will not
;; match the fat arrow operator '=>'
(defvar js-align--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\([^>]\\|$\\)\\|"
          (js--regexp-opt-symbol '("in" "instanceof"))))

(defun js-align--ternary-search (&optional jump)
  "Search backward for the context of a colon.
If JUMP skip over a matched ternary pair."
  (js--re-search-backward "[?:{}]\\|\\_<case\\_<" nil t)
  (cond ((eq (char-after) ?})
         (forward-char)
         (backward-sexp)
         (js-align--ternary-search))
        ((eq (char-after) ?:)
         (js-align--ternary-search t))
        ((eq (char-after) ??)
         (if jump
             (js-align--ternary-search)
           t))
        (t 'nil)))

(defun js-align--ternary-colon-p ()
    "Return non-nil if the colon at point is in a ternary expression"
    (message "working")
    (save-excursion
      (js-align--ternary-search)))

(defun js-align--terminal-arrow-p ()
  "Return non-nil if the line ends with an fat arrow."
  (save-excursion
    (goto-char (point-at-eol))
    (and (js--re-search-backward "[,(]\\|=>" (point-at-bol) t)
         (looking-at (concat "=>" js-align--empty-re)))))

;; This is base on the code `js--looking-at-operator-p' from `js.el'.
(defun js-align--looking-at-indent-operator-p ()
  "Return non-nil if point is on a JavaScript operator requiring indentation."
  (save-match-data
    (and (looking-at js-align--indent-operator-re)
         ;; exclude a colon if used outside a ternary expressions
         (or (not (eq (char-after) ?:))
             (js-align--ternary-colon-p))
         ;; looking back to catch ++ -- /* */ =>
         (or (not (memq (char-before) '(?- ?+ ?* ?/ ?=)))
             (save-excursion
               (backward-char)
               (looking-at js-align--indent-operator-re)))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re " *(\\)"))
               (save-excursion
                 (js--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{))))))))

;; This is base on the code `js--continued-expression-p' from `js.el'.
(defun js-align--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (js-align--looking-at-indent-operator-p)
        ;; + and - are also used as unary operators which are not
        ;; continued expressions
        (or (not (memq (char-after) '(?- ?+)))
            (progn
              (forward-comment (- (point)))
              (not (memq (char-before) '(?, ?\[ ?\()))))
      ;; the current line did not tigger a continued expression though
      ;; must look above in case the last expression was
      (and (js--re-search-backward "\n" nil t)
           (progn
             (skip-chars-backward " \t")
             (or (bobp) (backward-char))
             (and (> (point) (point-min))
                  (js-align--looking-at-indent-operator-p)))))))

;; This is base on the code `js--proper-indentation' from `js.el'.
(defun js-align--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((js--ctrl-statement-indentation))
          ;; ((js--multi-line-declaration-indentation)) ; disabled
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((status '())
                 (same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js-align--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ;go to the opening char
             (if (or (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (js-align--terminal-arrow-p))
                 (progn 
                   ;; nothing following the opening paren/bracket
                   ;; except maybe a fat arrow
                   (skip-syntax-backward " ")
                   ;; Is this to walk back across an argument list?
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   ;; (js--maybe-goto-declaration-keyword-end) ; disabled
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 js-indent-level)
                                     js-expr-indent-offset))
                                 (t
                                  (+ (current-column) js-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( js-paren-indent-offset)
                                       (?\[ js-square-indent-offset)
                                       (?\{ js-curly-indent-offset)))))))

                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

(provide 'js-align)
;;; js-align.el ends here
