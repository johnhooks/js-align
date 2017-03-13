;;; tests/indent.el --- Some tests of regular expressions of js-align.el

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

;; Shamelessly copied from github.com/mooz/js2-mode/tests/indent.el
;; `js2-test-indent'
(defun js-align-test-indent (content keep-indent)
  (let ((s (replace-regexp-in-string "^ *|" "" content)))
    (with-temp-buffer
      (insert
       (if keep-indent
           s
         (replace-regexp-in-string "^ *" "" s)))
      (js-mode)
      ;; Think this can be simplified
      (advice-add #'js--proper-indentation :override #'js-align--proper-indentation)
      (indent-region (point-min) (point-max))
      (should (string= s (buffer-substring-no-properties
                          (point-min) (point)))))))

(cl-defmacro js-align-deftest-indent (name content &key bind keep-indent)
  `(ert-deftest ,(intern (format "js-align-%s" name)) ()
     (let ,(append '((js-indent-level 2))
                   bind)
       (js-align-test-indent ,content ,keep-indent))))

(js-align-deftest-indent continued-expr-above
  "let foo = bar +
  |  baz")

(js-align-deftest-indent continued-expr-above-comment-a
  "let foo = bar + // Beware the Jabberwock, my son!
  |  baz")

(js-align-deftest-indent continued-expr-above-comment-b
  "let foo = bar + /* The jaws that bite, the claws that catch! */
  |  baz")

(js-align-deftest-indent continued-expr-below
  "let foo = 5
  |  + 4")

(js-align-deftest-indent continued-expr-below-comment-a
  "let foo = bar // Beware the Jubjub bird, and shun
  |  + baz")

(js-align-deftest-indent continued-expr-below-comment-b
  "let foo = bar /* The frumious Bandersnatch! */
  |  + baz")

(js-align-deftest-indent multi-line-arrow-declare
  "let foo = (bar) =>
  |  bar + bar")

(js-align-deftest-indent multi-line-arrow-in-args
  "foo((bar) =>
  |  bar + bar
  |)")

(js-align-deftest-indent continued-expr-sub-chain
  "let foo = (bar) =>
  |  bar
  |    .baz()")

(js-align-deftest-indent continued-expr-sub-chain-comment-a
  "let foo = (bar) =>
  |  bar // He took his vorpal sword in hand:
  |    .baz()")

(js-align-deftest-indent continued-expr-sub-chain-comment-b
  "let foo = (bar) =>
  |  bar /* Long time the manxome foe he sought -- */
  |    .baz()")

(js-align-deftest-indent ternary-expr-basic
  "let foo = (true)
  |  ? bar
  |  : baz")

(js-align-deftest-indent ternary-expr-object-literal
  "let foo = (true)
  |  ? {a: 2, b: b}
  |  : {a: a, b: 3}")

(js-align-deftest-indent ternary-expr-double-deep
  "let brillig = (slithy || toves)
  |  ? (gyre && gimble)
  |    ? wabe
  |    : mimsy
  |  : (borogoves())
  |    ? mome
  |    : raths")

(js-align-deftest-indent multi-line-strings-noop
  "`multiline strings
  |        contents
  |     are kept
  |   unchanged`"
  :keep-indent t)

(js-align-deftest-indent default-keyword-as-property
  "var foo = {
  |  case: 'not in switch',
  |  default: 'should indent like property'
  |}"
  :bind ((js-switch-indent-offset 0)))

(js-align-deftest-indent case-inside-switch
  "switch (true) {
  |case 'true':
  |  return false
  |default:
  |  return true
  |}"
  :bind ((js-switch-indent-offset 0)))

(js-align-deftest-indent case-inside-switch-offset
  "switch (true) {
  |  case 'true':
  |    return false
  |  default:
  |    return true
  |}"
  :bind ((js-switch-indent-offset 2)))

(js-align-deftest-indent case-inside-switch-with-extra-indent-curly-after-newline
  "switch (true)
  |{
  |  case 'true':
  |    return false
  |}"
  :bind ((js-switch-indent-offset 2)))
