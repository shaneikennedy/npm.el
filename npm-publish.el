;;; npm-publish.el --- Run your npm workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

;; Author: Shane Kennedy, Manas Jayanth
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
;; Functions for publishing project an NPM registry

;;; Code:
(require 'npm-common)

(transient-define-prefix npm-publish-menu ()
  "Open npm publish transient menu pop up."
    ["Arguments"
     ("-r" "set npm registry where the package must be published"        "--registry=")
     ("-d" "dry run"        "--dry-run")
     ("-o" "otp"        "--otp=")
     ("-w" "set workspace"        "--workspace=")
     ("-s" "set workspaces"        "--workspaces")
     ("-i" "include workspace root in the publish"        "--include-workspace-root")
     ("-a" "set package access for other users on the registry"        "--access=")
     ("-t" "set npm dist tag. See https://docs.npmjs.com/cli/v8/commands/npm-dist-tag"        "--tag=")
    ]
    [["Command"
      ("p" "Publish"       npm-publish)]]
  (interactive)
  (transient-setup 'npm-publish-menu))

(defconst npm-publish--prefix-command "npm publish")

(defun npm-publish--get-publish-command (tarball-name)
  "Construct the shell command for a given TARBALL-NAME."
  (concat npm-publish--prefix-command " " tarball-name))

(defun npm-publish--choose-tarball ()
  "Let user choose which package to publish."
  (interactive)
  (read-file-name "Type the path of the tarball you want to publish: " ()))

(defun npm-publish-menu-arguments nil
  "Arguments function for transient."
  (transient-args 'npm-publish-menu))

;;;###autoload
(defun npm-publish (&optional args)
  "Invoke the compile mode with ARGS to publish an NPM package to a registry."
   (interactive (list (npm-publish-menu-arguments)))
   (let* ((arguments (string-join args " "))
          (npm-command (npm-publish--get-publish-command (npm-publish--choose-tarball))))
     (npm-common--compile npm-command arguments)))

(provide 'npm-publish)
;;; npm-publish.el ends here
