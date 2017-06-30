/**
 * js-align/bin/build-idenifier.js
 *
 * Used to generate lists characters in order to generate syntax
 * tables for Emacs.
 *
 * Inspired by acorn/bin/generate-identifier-regex.js
 */

// Which Unicode version should be used?
const fs = require('fs')
const version = '9.0.0'

const start = require('unicode-' + version + '/Binary_Property/ID_Start/code-points.js')
    .filter(ch => ch > 0x7f)

let last = -1

const part = [0x200c, 0x200d].concat(
  require('unicode-' + version + '/Binary_Property/ID_Continue/code-points.js')
    .filter(ch => ch > 0x7f && search(start, ch, last + 1) === -1)
)

var identifierStart = new Set(start)
var identifierPart = new Set()

// Populate identifierPart
for (var elem of new Set(part)) {
  if (!identifierStart.has(elem)) {
    identifierPart.add(elem)
  }
}

function search (arr, ch, starting) {
  for (var i = starting; arr[i] <= ch && i < arr.length; last = i++) {
    if (arr[i] === ch) {
      return i
    }
  }
  return -1
}

function hexify (code) {
  return `#x${code.toString(16)}`
}

function generate (chars) {
  let list = ''
  for (let i = 0; i < chars.length; i++) {
    let from = chars[i]
    let to = from
    while (i < chars.length - 1 && chars[i + 1] === to + 1) {
      i++
      to++
    }
    // An extra space ends up in the first item in the list.
    if (from === to) {
      list += ' ' + hexify(from)
    } else if (from + 1 === to) {
      list += ` ${hexify(from)} ${hexify(to)}`
    } else {
      list += ` (${hexify(from)} . ${hexify(to)})`
    }
  }
  return list
}

console.log(
`;;;; js-align-identifier.el --- JavaScript Align Syntax Identifiers -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Generated using code inspired by acorn/bin/generate-identifier-regex.js

;;; Code:

(defconst js-align-syntax-word-chars '(${generate([... identifierStart])})
"List of identifier start characters and character ranges.")

(defconst js-align-syntax-symbol-chars '(${generate([... identifierPart])})
"List of identifier part characters and character ranges.")

(provide 'js-align-identifier)

;;; js-align-identifier.el ends here`)
