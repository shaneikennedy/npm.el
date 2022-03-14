;;; npm.el --- Run your npm workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

;; Author: Shane Kennedy
;; Homepage: https://github.com/shaneikennedy/npm.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0") (jest "20200625"))
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
(require 'jest)

(require 'npm-common)
(require 'npm-run)
(require 'npm-install)
(require 'npm-update)
(require 'npm-init)
(require 'npm-publish)

(defconst npm-mode-map compilation-mode-map)

(define-derived-mode npm-mode compilation-mode "NPM"
  "Major mode for the NPM compilation buffer."
  (use-local-map npm-mode-map)
  (setq major-mode 'npm-mode)
  (setq mode-name "NPM")
  (setq-local truncate-lines t))

;;;###autoload
(defun npm ()
  "Entrypoint function to the package.
This will first check to make sure there is a package.json file and then open the menu."
  (interactive)
  (if (npm-common--get-project-dir)
      (call-interactively #'npm-menu)
      (if (y-or-n-p "You are not in an NPM project, would you like to initialize one? ")
          (call-interactively #'npm-init))))

;; Entrypoint menu
(transient-define-prefix npm-menu ()
  "Open npm transient menu pop up."
    [["Command"
      ("u" "Update"       npm-update)
      ("i" "Install"       npm-install-menu)
      ("r" "Run"       npm-run)
      ("t" "Test"       jest-popup)
      ("p" "Publish"       npm-publish-menu)]]
  (interactive)
  (transient-setup 'npm-menu))


(provide 'npm)
;;; npm.el ends here
