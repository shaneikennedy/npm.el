;;; npm-install.el --- Run your npm workflows -*- lexical-binding: t; -*-

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
;; Functions for installing npm pakcages.

;;; Code:
(require 'npm-common)

(transient-define-prefix npm-install-menu ()
  "Open npm install transient menu pop up."
    ["Arguments"
     ("-f" "Force fetching even if copy exists on disk"        "--force")
     ("-g" "Save as global dependency"        "--global")
     ("-p" "Save as production dependency"        "--save-prod")
     ("-d" "Save as development dependency"        "--save-dev")
     ("-o" "Save as optional dependency"        "--save-optional")
     ("-n" "Do not save to package.json"        "--no-save")]
    [["Command"
      ("i" "Install"       npm-install)]]
  (interactive)
  (transient-setup 'npm-install-menu))


(defconst npm-install--prefix-command "npm install")

(defun npm-install--get-install-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat npm-install--prefix-command " " package-name))

(defun npm-install--choose-package ()
  "Let user choose which package to install."
  (interactive)
  (completing-read "Type the name of the package you want to install: " ()))

(defun npm-install-menu-arguments nil
  "Arguments function for transient."
  (transient-args 'npm-install-menu))


;;;###autoload
(defun npm-install (&optional args)
  "Invoke the compile mode with the install prefix-command and ARGS if provided."
  (interactive (list (npm-install-menu-arguments)))
  (let* ((arguments (string-join args " "))
         (npm-command (npm-install--get-install-command (npm-install--choose-package))))
    (npm-common--compile npm-command arguments)))

(provide 'npm-install)
;;; npm-install.el ends here
