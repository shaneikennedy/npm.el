;;; npm-update.el --- Run your npm workflows -*- lexical-binding: t; -*-

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
;;; Functions for running npm update.

;;; Code:
(require 'npm-common)

(defconst npm-update--prefix-command "npm update")

(defun npm-update--get-update-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat npm-update--prefix-command " " package-name))

(defun npm-update--get-packages (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm packages."
  (append
   (npm-update--get-dev-dependency-packages project-dir)
   (npm-update--get-optional-dependency-packages project-dir)
   (npm-update--get-dependency-packages project-dir)))

(defun npm-update--get-dev-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm devDependencies."
  (mapcar 'car (cdr (assoc 'devDependencies (json-read-file (concat project-dir npm-common--config-file))))))

(defun npm-update--get-optional-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm optionalDependencies."
  (mapcar 'car (cdr (assoc 'optionalDependencies (json-read-file (concat project-dir npm-common--config-file))))))

(defun npm-update--get-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm dependencies."
  (mapcar 'car (cdr (assoc 'dependencies (json-read-file (concat project-dir npm-common--config-file))))))

(defun npm-update--choose-package ()
  "Let user choose which package to update."
  (interactive)
  (completing-read "Select package from list: " (npm-update--get-packages (npm-common--get-project-dir)) nil t))

;;;###autoload
(defun npm-update (&optional _args)
  "Invoke the compile mode with the update prefix-command and ARGS if provided."
  (interactive (list (npm-common--arguments)))
  (npm-common--compile (npm-update--get-update-command (npm-update--choose-package))))


(provide 'npm-update)
;;; npm-update.el ends here
