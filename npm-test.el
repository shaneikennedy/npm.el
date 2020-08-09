;;; npm-run.el --- Run your npm workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

;; Author: Shane Kennedy
;; Homepage: https://github.com/shaneikennedy/npm.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'npm-common)

;; NPM TEST
(defconst npm-test--prefix-command "npm test")

(defun npm-test (&optional _args)
  "Invoke the compile mode with the test prefix-command and ARGS if provided."
  (interactive (list (npm-arguments)))
  (npm-compile npm-test--prefix-command))

(provide 'npm-test)
;;; npm-test.el ends here
