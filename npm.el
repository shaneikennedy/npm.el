;;; npm -- Emacs package for running npm scripts

;;; Commentary:

;;; Code:
(require 'json)
(require 'compile)
(require 'transient)

(defconst package-json-file "package.json")

;; Common
(defun get-project-dir ()
  "Function that determines the file path of the project root directory."
  (locate-dominating-file (buffer-file-name) package-json-file))


;; NPM RUN
(defconst npm-run-prefix "npm run")

(defun npm-run--get-run-command (script-name)
  "Construct the shell command for a given SCRIPT-NAME."
  (concat npm-run-prefix " " script-name))

(defun npm-run--get-scripts (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm scripts."
  (cdr (assoc 'scripts (json-read-file (concat project-dir package-json-file)))))


(defun npm-run--choose-script ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (npm-run--get-scripts (get-project-dir)) nil t))

(defun npm-run--command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (get-project-dir)))
          (command (npm-run--get-run-command (npm-run--choose-script))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; NPM TEST
(defconst npm-test-command "npm test")

(defun npm-test--command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (get-project-dir)))
          (command 'npm-test-command))
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
  "Let user choose which script to run."
  (interactive)
  (completing-read "Type the name of the package you want to install: " ()))

(defun npm-install--command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (get-project-dir)))
          (command (npm-install--get-install-command (npm-install--choose-package))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; NPM UPDATE
(defconst npm-update--prefix-command "npm update")

(defun npm-update--get-update-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat npm-update--prefix-command " " package-name))

(defun npm-update--get-packages (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find npm scripts."
  (cdr (assoc 'dependencies (json-read-file (concat project-dir package-json-file)))))

(defun npm-update--choose-package ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (npm-update--get-packages (get-project-dir)) nil t))

(defun npm-update--command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided."
  (interactive (list (npm-arguments)))
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (get-project-dir)))
          (command (npm-update--get-update-command (npm-update--choose-package))))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (call-interactively 'compile)
      (kill-buffer project-root-folder))))


;; Transient menus

;; Entrypoint menu
(define-transient-command npm ()
  "Open npm transient menu pop up."
    [["Command"
      ("u" "Update"       npm-update--command)
      ("i" "Install"       npm-install--command)
      ("r" "Run"       npm-run--command)
      ("t" "Test"       npm-test--command)]]
  (interactive)
  (transient-setup 'npm))

(defun npm-arguments nil
  "Arguments function for transient."
  (transient-args 'npm))

(provide 'npm)
;;; npm.el ends here
