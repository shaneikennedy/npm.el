;;; npm.el --- Run your npm workflows from inside emacs  -*- lexical-binding: t; -*-

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
(require 'json)
(require 'compile)
(require 'transient)

(defconst npm-config-file "package.json")

;; Common
(defun npm-get-project-dir ()
  "Function that determines the file path of the project root directory."
  (locate-dominating-file (buffer-file-name) npm-config-file))


;; NPM RUN
(defconst npm-run--prefix-command "npm run")

(defun npm-run--get-run-command (script-name)
  "Construct the shell command for a given SCRIPT-NAME."
  (concat npm-run--prefix-command " " script-name))

(defun npm-run--get-scripts (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm scripts."
  (cdr (assoc 'scripts (json-read-file (concat project-dir npm-config-file)))))


(defun npm-run--choose-script ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (npm-run--get-scripts (npm-get-project-dir)) nil t))

(defun npm-run--command (&optional args)
  "Invoke the compile mode with the run prefix-command and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (npm-get-project-dir)))
          (command (npm-run--get-run-command (npm-run--choose-script))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; NPM TEST
(defconst npm-test--prefix-command "npm test")

(defun npm-test--command (&optional args)
  "Invoke the compile mode with the test prefix-command and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (npm-get-project-dir)))
          (command 'npm-test--prefix-command))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; NPM INSTALL
(defconst npm-install--prefix-command "npm install")

(defun npm-install--get-install-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat npm-install--prefix-command " " package-name))

(defun npm-install--choose-package ()
  "Let user choose which package to install."
  (interactive)
  (completing-read "Type the name of the package you want to install: " ()))

(defun npm-install--command (&optional args)
  "Invoke the compile mode with the install prefix-command and ARGS if provided."
  (interactive (list (npm-install-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (npm-get-project-dir)))
           (arguments (string-join args " "))
           (command (npm-install--get-install-command (npm-install--choose-package))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command (string-join (list command arguments) " "))
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


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

(defun npm-update--command (&optional args)
  "Invoke the compile mode with the update prefic-command and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (npm-get-project-dir)))
          (command (npm-update--get-update-command (npm-update--choose-package))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; Transient menus
(define-transient-command npm-install ()
  "Open npm install transient menu pop up."
    ["Arguments"
     ("-f" "Force fetching even if copy exists on disk"        "--force")
     ("-g" "Save as global dependency"        "--global")
     ("-p" "Save as production dependency"        "--save-prod")
     ("-d" "Save as development dependency"        "--save-dev")
     ("-o" "Save as optional dependency"        "--save-optional")
     ("-n" "Do not save to package.json"        "--no-save")]
    [["Command"
      ("i" "Install"       npm-install--command)]]
  (interactive)
  (transient-setup 'npm-install))

(defun npm-install-arguments nil
  "Arguments function for transient."
  (transient-args 'npm-install))

;; Entrypoint menu
(define-transient-command npm ()
  "Open npm transient menu pop up."
    [["Command"
      ("u" "Update"       npm-update--command)
      ("i" "Install"       npm-install)
      ("r" "Run"       npm-run--command)
      ("t" "Test"       npm-test--command)]]
  (interactive)
  (transient-setup 'npm))

(defun npm-arguments nil
  "Arguments function for transient."
  (transient-args 'npm))

(provide 'npm)
;;; npm.el ends here
