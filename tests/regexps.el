;;; tests/regexps.el --- Some tests of regular expressions of js-align.el

;;; Copyright (C) 2017 John Hooks

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

;;; The Code:

(require 'cl)
(require 'ert)
(require 'js-align)

(defmacro test-op-re (operator name &optional fail)
  `(ert-deftest ,(intern (format "test-op-re-%s" name)) ()
     (with-temp-buffer
       (insert ,operator)
       (goto-char (point-min))
       (should (equal (looking-at js-align--indent-operator-re)
                      (not ,fail))))))

(defmacro test-empty-re (string name &optional fail)
  `(ert-deftest ,(intern (format "test-empty-re-%s" name)) ()
     (with-temp-buffer
       (insert ,string)
       (goto-char (point-min))
       (should (equal (looking-at js-align--empty-re)
                      (not ,fail))))))

;; `js-align--empty-re' Match Tests

(test-empty-re "" "eol")
(test-empty-re "//" "single-line-comment")
(test-empty-re "/*" "multi-line-comment")
(test-empty-re "    " "eol-whitespace")
(test-empty-re "    //" "single-line-comment--whitespace")
(test-empty-re "    /*" "multi-line-comment--whitespace")

;; `js-align--empty-re' Failure Tests

(test-empty-re "foo" "with-content" t)
(test-empty-re "bar //" "with-content-single-line-comment" t)
(test-empty-re "baz /*" "with-content-multi-line-comment" t)

;; `js-align--indent-operator-re' Match Tests

(test-op-re "+" "addition")
(test-op-re "-" "subtraction")
(test-op-re "*" "multiplication")
(test-op-re "/" "division")
(test-op-re "%" "remainder")
(test-op-re "<" "less-than")
(test-op-re ">" "greater-than")
(test-op-re "&" "bitwise-and")
(test-op-re "|" "bitwise-or")
(test-op-re "^" "bitwise-xor")
(test-op-re "?" "conditional-?")
(test-op-re ":" "conditional-:")
(test-op-re "=" "assignment")
(test-op-re "." "member")
(test-op-re "&&" "logical-and")
(test-op-re "||" "logical-or")
;; (test-op-re "**" "exponentiation")
(test-op-re "==" "equal")
(test-op-re "!=" "not-equal")
(test-op-re "<=" "less-than-or-equal")
(test-op-re ">=" "greater-than-or-equal")
(test-op-re "+=" "assignment-addition")
(test-op-re "-=" "assignment-subtraction")
(test-op-re "*=" "assignment-mutliplication")
(test-op-re "/=" "assignment-division")
(test-op-re "%=" "assignment-remainder")
(test-op-re "&=" "assignment-bitwise-and")
(test-op-re "^=" "assignment-bitwise-xor")
(test-op-re "|=" "assignment-bitwise-or")
(test-op-re "<<" "left-shift")
(test-op-re ">>" "right-shift")
(test-op-re ">>>" "unsigned-right-shift")
;; (test-op-re "**=" "assignment-exponentiation")
(test-op-re "<<=" "assignment-left-shift")
(test-op-re ">>=" "assignment-right-shift")
(test-op-re "===" "strict-equal")
(test-op-re "!==" "strict-not-equal")
(test-op-re ">>>=" "assignment-unsigned-right-shift")

;; `js-align--indent-operator-re' Failure Tests
(test-op-re "!" "logical-not" t)
(test-op-re "~" "bitwise-not" t)
(test-op-re "++" "increment" t)
(test-op-re "--" "decrement" t)
(test-op-re "=>" "fat-arrow" t)
(test-op-re "/*" "open-comment" t)
(test-op-re "*/" "close-comment" t)
(test-op-re "//" "single-comment" t)
