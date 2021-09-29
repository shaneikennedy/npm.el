;;; npm-init.el --- Run your npm workflows -*- lexical-binding: t; -*-

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
;; Functions for initializing a node project.

;;; Code:
(require 'npm-common)

(defconst npm-init--prefix-command "npm init")
(defconst npm-init--temp-buffer ".npminit")

;;;###autoload
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

(provide 'npm-init)
;;; npm-init.el ends here
