
;;; js-align.el --- JavaScript Indentation

;; Copyright (C) 2016 John Hooks
;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

;; Author: John Hooks <john@bitmachina.com>

;; Maintainer: John Hooks <john@bitmachina.com>
;; URL: https://github.com/johnhooks/js-align
;; Version: 0.2.0

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

;; Exported names start with "js-align"; private names start with
;; "js-align--".

;;; Code:

(require 'js)
(require 'js-align-identifier)
(require 'js-align-polyfill)

(defgroup js-align-mode nil
  "Improved JavaScript Indenation."
  :group 'languages)

(defvar js-align--syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry '(?0 . ?9) "_" table)
    (dolist (char js-align-syntax-word-chars)
      (modify-syntax-entry char "w" table))
    (dolist (char js-align-syntax-symbol-chars)
      (modify-syntax-entry char "_" table))
    table)
  "Syntax table for `js-align-mode'.
The word syntax class is used for JavaScript Identifier start characters,
and the symbol syntax class is used for Identifier part characters ")

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

(defun js-align--word-or-symbol-p (char)
  ;; 95 symbol constituent "_"
  ;; 119 word constituent "w"
  (or (eq (char-syntax char) 95)
      (eq (char-syntax char) 119)))

(defun js-align--backward-token (&optional jump)
  "Move backward one token.
Returns a symbol representing the type of token or nil if there is not
a token before point. If JUMP is non-nil, matched sets of parens, curly
and square braces will be jumped over to the opening punctuation."

  ;; Should not have to worry about beginning search from inside a
  ;; string or comment because `js-align--proper-indentation' will
  ;; handle this situation. Otherwise `backward-sexp' will possibly
  ;; jump unmatched pairs.

  ;; move backwards skipping comments and whitespace
  (forward-comment most-negative-fixnum)
  (when (not (eq (point) (point-min)))
    (let ((syntax-code (char-syntax (char-before)))
          (end (point)))
      (cond ((memq syntax-code '(95 119)) ; 95 symbol constituent "_"
             (skip-syntax-backward "w_")  ; 119 word constituent "w"
             (let ((on (char-after)))
               (if (and (> on 47) (< on 58))  ; ansi number codes
                   'number
                 'symbol)))
            ((eq syntax-code 46) ; 46 punctuation character "."
             (skip-syntax-backward ".")
             'operator)
            ((eq syntax-code 41) ; 41 close delimiter character ")"
             (if jump
                 (backward-sexp)
               (backward-char))
             'close)
            ((eq syntax-code 40) ; 40 open delimiter character "("
             (backward-char)
             'open)
            ((eq syntax-code 34) ; 34 string quote character "\""
             (backward-sexp)
             'string)
            (t nil)))))

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

(defun js-align--beginning-of-call ()
  "Move point to the beginning of a function call.
This function walks the method chain if necessary. Moves point to the
beginning of the function call or the object on which the method chain
begun and return non-nil. If an anomaly is encountered, return nil and
leave point at the original position.  Needs to be called from outside
the argument list."
  (let (pos
        (looking t)
        (beginning (point)))
    (while looking
      (forward-comment most-negative-fixnum)
      (if (not (eq (point) (point-min)))
          (progn
            (when (eq (char-before) ?\))
              (backward-list))           ; jump argument list
            (when (js-align--word-or-symbol-p (char-before))
              (skip-syntax-backward "w_")
              (and (looking-at js--name-start-re)
                   (setq looking nil)))  ; maybe found it?
            (cond ((eq (char-before) ?.) ; member operator
                   (setq looking t)      ; nope didn't fine it
                   (backward-char))
                  ((null looking)
                   (setq pos (point)))   ; found it!
                  (t
                   (setq looking nil)))) ; failure, exit loop
        (setq looking nil)))             ; hit bob, exit loop
    (if pos
        t
      (progn (goto-char beginning)
             nil))))

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
         ;; Fail on a colon if used outside a ternary expression.
         (or (not (eq (char-after) ?:))
             (js-align--ternary-search))
         ;; Catch forward slash in regular expression rather than division.
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         ;; Looking back to catch ++ -- /* */ =>
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
    (and (js-align--backward-operator)
         (looking-at "=>")
         (+ (current-indentation) js-indent-level))))

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
          ((and (eq (char-after) ?.)
                (js-align--beginning-of-call)
                (progn
                  (back-to-indentation)
                  (+ (current-column) js-indent-level
                     js-expr-indent-offset))))
          ((js-align--arrow-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js-align--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ;go to the opening char
             (if (or (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (js-align--terminal-arrow-p))
                 (progn
                   ;; nothing following the opening paren/bracket
                   ;; except maybe a fat arrow
                   (js-align--backward-token t)
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
          ((or (js-align--continued-expression-p))
           (progn
             (+ js-indent-level js-expr-indent-offset)))
          (t 0))))

(defun js-align-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (js-align--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(define-minor-mode js-align-mode
  "Minor mode for improved JavaScript indentation."
  :lighter " align"
  :group js-align-mode
  (if js-align-mode
      (js-align-mode-enter)
    (js-align-mode-exit)))

(defun js-align-mode-enter ()
  "Initialization for `js-align-mode'."
  ;; Add The advice to `js-mode' to replace the indentation function
  (setq-local indent-line-function #'js-align-indent-line)
  (setq-local syntax-propertize-function #'--js-syntax-propertize))

(defun js-align-mode-exit ()
  "Turn off `js-align-mode'."
  ;; Remove the advice from `js-mode' to restore original function
  (setq-local indent-line-function #'js-indent-line)
  (setq-local syntax-propertize-function #'js-syntax-propertize))

(provide 'js-align)
;;; js-align.el ends here
