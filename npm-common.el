;;; npm-common.el --- Run your npm workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

;; Author: Shane Kennedy
;; Homepage: https://github.com/shaneikennedy/npm.el
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
;; The common functions needed by many or all npm commands.

;;; Code:
(require 'compile)
(require 'json)
(require 'subr-x)
(require 'transient)

(defconst npm-common--config-file "package.json")

;; Common
(defun npm-common--get-project-dir ()
  "Function that determines the file path of the project root directory."
  (locate-dominating-file (buffer-file-name) npm-common--config-file))

(defun npm-common--compile (npm-command &optional args)
  "Generic compile command for NPM-COMMAND with ARGS functionality."
  (let ((buffer-name "*npm*"))
    (compilation-start (string-join (list npm-command args) " ") 'npm-mode)
    (with-current-buffer "*npm*" (rename-buffer buffer-name))))

(defun npm-common--arguments nil
  "Arguments function for transient."
  (transient-args 'npm-menu))

(provide 'npm-common)
;;; npm-common.el ends here
