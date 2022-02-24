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

(defgroup npm ()
  "Group for npm."
  :group 'tools
  :prefix "npm-")

(defconst npm-common--config-file "package.json")

(defcustom npm-common-buffer-name-function "*npm*"
  "Buffer name for `npm' command, or function which return buffer name.
The function takes three arguments, ROOT, NPM-COMMAND, ARGS.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

You can use `npm-common-create-unique-buffer-name' to use unique buffer name
among all sesstions."
  :group 'npm
  :type '(choice
          (string :tag "Use same buffer through all sessions")
          (const :tag "Use unique buffer name among all sessions" npm-common-create-unique-buffer-name)
          function))

(defun npm-common-create-unique-buffer-name (root npm-command _args)
  "Create buffer name unique to ROOT and NPM-COMMAND."
  (concat "*" npm-command " in " root "*"))

(defun npm-common--generate-buffer-name-function (root npm-command args)
  "Generate function which return buffer name to pass `compilation-start'.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

This function uses `npm-common-buffer-name-function'."
  (lambda (_)
    (if (stringp npm-common-buffer-name-function)
        npm-common-buffer-name-function
      (funcall npm-common-buffer-name-function
               root npm-command args))))

;; Common
(defun npm-common--get-project-dir ()
  "Function that determines the file path of the project root directory."
  (locate-dominating-file (or (buffer-file-name) default-directory)
                          npm-common--config-file))

(defun npm-common--compile (npm-command &optional args)
  "Generic compile command for NPM-COMMAND with ARGS functionality."
  (compilation-start (string-join (list npm-command args) " ")
                     'npm-mode
                     (npm-common--generate-buffer-name-function
                      (npm-common--get-project-dir) npm-command args)))

(defun npm-common--arguments nil
  "Arguments function for transient."
  (transient-args 'npm-menu))

(provide 'npm-common)
;;; npm-common.el ends here
