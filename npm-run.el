;;; npm-run.el --- Run your npm workflows -*- lexical-binding: t; -*-

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
;; Functions for using npm run.

;;; Code:
(require 'npm-common)

(defconst npm-run--prefix-command "npm run")

(defun npm-run--get-run-command (script-name)
  "Construct the shell command for a given SCRIPT-NAME."
  (concat npm-run--prefix-command " " script-name))

(defun npm-run--get-scripts (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm scripts."
  (mapcar 'car (cdr (assoc 'scripts (json-read-file (concat project-dir npm-common--config-file))))))


(defun npm-run--choose-script ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (npm-run--get-scripts (npm-common--get-project-dir)) nil t))

;;;###autoload
(defun npm-run (&optional _args)
  "Invoke the compile mode with the run prefix-command and ARGS if provided."
  (interactive (list (npm-common--arguments)))
  (npm-common--compile (npm-run--get-run-command (npm-run--choose-script))))

(provide 'npm-run)
;;; npm-run.el ends here
