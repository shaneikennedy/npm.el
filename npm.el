;;; npm -- Emacs package for running npm scripts

;;; Commentary:

;;; Code:
(require 'json)
(require 'compile)
(require 'transient)

(defconst package-json-file "package.json")
(defconst npm-run-prefix "npm run")

(defun get-npm-run-command (script-name)
  "Construct the shell command for a given SCRIPT-NAME."
  (concat npm-run-prefix " " script-name))

(defun get-npm-scripts (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm scripts."
  (cdr (assoc 'scripts (json-read-file (concat project-dir package-json-file)))))

(defun get-project-dir ()
  "Function that determines the file path of the project root directory."
  (locate-dominating-file (buffer-file-name) package-json-file))

(defun npm-choose-script ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (get-npm-scripts (get-project-dir)) nil t))

(defun npm-run-command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (get-project-dir)))
          (command (get-npm-run-command (npm-choose-script))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))

(define-transient-command npm ()
  "Open npm transient menu pop up."
    [["Command"
    ("r" "Run"       npm-run-command)]]
  (interactive)
  (transient-setup 'npm))

(defun npm-arguments nil
  "Arguments function for transient."
  (transient-args 'npm))

(provide 'npm)
;;; npm.el ends here
