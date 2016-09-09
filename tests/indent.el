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

(js-align-deftest-indent multi-line-arrow-declare
  "let foo = (x) =>
  |  x * x")

(js-align-deftest-indent multi-line-arrow-in-args
  "foo((x) =>
  |  x * x
  |)")

(js-align-deftest-indent continued-expression-above
  "let foo = 5 +
  |  4")

(js-align-deftest-indent continued-expression-below
  "let foo = 5
  |  + 4")

(js-align-deftest-indent multiline-strings-noop
  "`multiline strings
  |        contents
  |     are kept
  |   unchanged`"
  :keep-indent t)

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
