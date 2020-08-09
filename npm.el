;;; npm.el --- Run your npm workflows -*- lexical-binding: t; -*-

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
;; This package offers a transient interface to the npm cli.

;;; Code:
(require 'compile)
(require 'json)
(require 'subr-x)
(require 'transient)
(require 'npm-common)
(require 'npm-run)
(require 'npm-test)
(require 'npm-install)

(defun npm ()
  "Entrypoint function to the package.
This will first check to make sure there is a package.json file and then open the menu."
  (interactive)
  (if (npm-get-project-dir)
      (call-interactively #'npm-menu)
      (if (y-or-n-p "You are not in an NPM project, would you like to initialize one? ")
          (call-interactively #'npm-init))))

(defconst npm-mode-map compilation-mode-map)

(define-derived-mode npm-mode compilation-mode "NPM"
  "Major mode for the NPM compilation buffer."
  (use-local-map npm-mode-map)
  (setq major-mode 'npm-mode)
  (setq mode-name "NPM")
  (setq-local truncate-lines t))

;; NPM UPDATE
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
  (cdr (assoc 'devDependencies (json-read-file (concat project-dir npm-config-file)))))

(defun npm-update--get-optional-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm optionalDependencies."
  (cdr (assoc 'optionalDependencies (json-read-file (concat project-dir npm-config-file)))))

(defun npm-update--get-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm dependencies."
  (cdr (assoc 'dependencies (json-read-file (concat project-dir npm-config-file)))))

(defun npm-update--choose-package ()
  "Let user choose which package to update."
  (interactive)
  (completing-read "Select package from list: " (npm-update--get-packages (npm-get-project-dir)) nil t))

(defun npm-update (&optional _args)
  "Invoke the compile mode with the update prefix-command and ARGS if provided."
  (interactive (list (npm-arguments)))
  (npm-compile (npm-update--get-update-command (npm-update--choose-package))))


;; NPM INIT
(defconst npm-init--prefix-command "npm init")
(defconst npm-init--temp-buffer ".npminit")

(defun npm-init ()
  "Initialize a project folder as a npm project."
   (interactive)
   (save-excursion
     (let* ((project-root-folder (read-directory-name "Project root :"))
            (command npm-init--prefix-command))
      (generate-new-buffer (concat project-root-folder npm-init--temp-buffer))
      (set-buffer (concat project-root-folder npm-init--temp-buffer))
      (let ((current-prefix-arg '(4)))
        (setq compilation-read-command nil)
        (setq compile-command command)
        (call-interactively #'compile))
        (kill-buffer project-root-folder))))

;; Entrypoint menu
(define-transient-command npm-menu ()
  "Open npm transient menu pop up."
    [["Command"
      ("u" "Update"       npm-update)
      ("i" "Install"       npm-install-menu)
      ("r" "Run"       npm-run)
      ("t" "Test"       npm-test)]]
  (interactive)
  (transient-setup 'npm-menu))

(provide 'npm)
;;; npm.el ends here
