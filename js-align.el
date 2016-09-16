;;; js-align.el --- JavaScript Indentation

;; Copyright (C) 2016 John Hooks
;; Copyright (C) 2008-2016 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;;         John Hooks <john@bitmachina.com>

;; Maintainer: John Hooks <john@bitmachina.com>
;; URL: https://github.com/johnhooks/js-align
;; Version: 0.1.2

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

(defvar js-align--empty-re "\\s-*\\($\\|/[/*]\\|//\\)")

;; Based on `js--indent-operator-re' from `js.el'. Though will not
;; match the fat arrow operator '=>'
(defvar js-align--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\([^>]\\|$\\)\\|"
          (js--regexp-opt-symbol '("in" "instanceof"))))

(defvar js--align-assignment-re
  "\\([-+*/%&^|]\\|[*<>]\\{2\\}>?\\)?=$")

(defvar js-align--no-indent-operator-re
  "[-+*][-+/]\\|[/*]/")

(defun js-align--backward-operator ()
  "Move point to the beginning of the previous token if an operator.
Return non-nil if success."
  (let ((beginning (point)))
    (forward-comment most-negative-fixnum)
    (when (not (eq (point) (point-min)))
      (if (eq (char-syntax (char-before)) 46)
          (progn (skip-syntax-backward ".")
                 t)
        (progn (goto-char beginning)
               nil)))))

(defun js-align--ternary-search ()
  "Search backwards for the matched question mark of a ternary colon.
Skips over any subexpression ternary pairs. Returns the position of
the question mark operator or nil if not a ternary colon."
  (save-excursion
    (let (pos
          jump
          (looking t))
      (while looking
        (js--re-search-backward "[]}){,?:]\\|\\<case\\>\\|\\<default\\>" nil t)
        (cond ((memq (char-after) '(?\] ?\} ?\)))
               (forward-char)
               (backward-sexp))
              ((eq (char-after) ?:)
               (setq jump t))
              ((eq (char-after) ??)
               (if jump
                   (setq jump nil) ; found subexpression '?'
                 (setq pos (point) looking nil))) ; found it
              (t
               (setq looking nil))))
      pos)))

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
         ;; fail on a colon if used outside a ternary expression
         (or (not (eq (char-after) ?:))
             (js-align--ternary-search))
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
      (and (js-align--backward-operator)
           (js-align--looking-at-indent-operator-p)))))

(defun js-align--arrow-indentation ()
  "Return indentation of a multi line arrow function explicit return."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (js--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   ;; necessary to `forward-char' in order to move off
                   ;; the match to [:graph:]
                   (or (eobp) (forward-char))
                   ;; skip over syntax whitespace
                   (skip-syntax-backward " ")
                   ;; skip over syntax punctuation
                   (skip-syntax-backward ".")
                   ;; Looking at a fat arrow?
                   (looking-at "=>"))))
      ;; if looking at a fat arrow, move to the saved match location
      (goto-char (match-beginning 0))
      ;; return the indentation
      (+ (current-indentation) js-indent-level))))

(defun js-align--backward-jump (&optional start)
  "Move backward one token, jumping over delimiter pairs.
Returns an alist of data representing the token. Optionally START at
a position other than point."
  ;; alist keys include the following:
  ;;   end  - the position of the last char of the token or the
  ;;          position after the closing delimiter char
  ;;   type - the type of token
  ;;
  ;; Should not have to worry about beginning a search from inside a
  ;; string or comment because `js-align--proper-indentation' will
  ;; handle this situation. Otherwise `backward-sexp' will possibly
  ;; jump unmatched pairs of strings and delimiters.
  (when start (goto-char start))
  ;; move backwards skipping comments and whitespace
  (forward-comment most-negative-fixnum)
  (when (not (eq (point) (point-min)))
    (let (type
          (char (char-before))
          (end (point)))
      (cond ((eq (char-syntax char) 46) ; 46 punctuation character "."
             (skip-syntax-backward ".")
             (setq type 'operator))
            ((or (eq (char-syntax char) 95)   ; 95 symbol constituent "_"
                 (eq (char-syntax char) 119)) ; 119 word constituent "w"
             (skip-syntax-backward "w_")
             (let ((on (char-after)))
               (if (and (> on 47) (< on 58))  ; ansi number codes
                   (setq type 'number)
                 (setq type 'symbol))))
            ((eq (char-syntax char) 41) ; 41 close delimiter character ")"
             (backward-sexp)
             (setq type 'close))
            ((eq (char-syntax char) 40) ; 40 open delimiter character "("
             (backward-char)
             (setq type 'open))
            ((eq (char-syntax char) 34) ; 34 string quote character "\""
             (backward-sexp)
             (setq type 'string)))
      ;; Create alist of token data
      `((end ,end) (type ,type)))))

(defun js-align--backward-peek ()
  "Move backward one token. Returns an alist representing the token."
  ;; Adds following keys to alist returned by `js-align--backward-jump'
  ;; start - the position of the beginning of the token
  ;;         may be redundant.
  (save-excursion
    (let ((data (js-align--backward-jump)))
      (unless (null data)
        (append data `((start ,(point))))))))

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
           (let (
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

          ;; ** Issue **
          ;; when the arrow operator is inside a set of square braces,
          ;; curly braces, or parens it works best for it to almost be
          ;; considered its own block, though it does not work if it is
          ;; on its own as a single expression....
          ((js-align--arrow-indentation))
          ((or (js-align--continued-expression-p))
           (progn
             (+ js-indent-level js-expr-indent-offset)))
          (t 0))))

;; Add the addvice to `js-mode' to replace the indentation function
(advice-add #'js--proper-indentation :override #'js-align--proper-indentation)

(provide 'js-align)
;;; js-align.el ends here
