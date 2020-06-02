;;; gradle-mode.el --- Minor mode for running gradle build.

;;; Commentary:

;;; Code:
(require 'compile)

(defcustom gradle-run-build-args nil
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defcustom gradle-run-env-vars nil
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defvar gradle-run-gradlew-command (if (string= system-type "windows-nt") "gradlew.bat" "gradlew")
  "The gradle wrapper command.")

(defun gradle-run--get-root ()
  "Return the root directory of the gradle project."
  (let ((root-dir (or (and (featurep 'projectile) (projectile-project-p))
                      (locate-dominating-file default-directory gradle-run-gradlew-command))))
    (or root-dir
        (user-error "No gradle project to run"))))

(defun gradle-run-set-env ()
  "Promt to read the environment variables setting for gradle-run-env-vars."
  (interactive)
  (setq gradle-run-env-vars
        (read-from-minibuffer "Env variables: " gradle-run-env-vars)))

(defun gradle-run ()
  "Run gradlew command by interactively read arguments to run."
  (interactive)
  (setq gradle-run-build-args
        (read-from-minibuffer "Build args: " gradle-run-build-args))
  (gradle-run--exec gradle-run-build-args gradle-run-env-vars))

(defun gradle-run-build ()
  "Run gradle build task."
  (interactive)
  (gradle-run--exec "build" gradle-run-env-vars))

(defun gradle-run--exec (build-args env-vars)
  "Run gradle with the specified build arguments and environment variables.

BUILD-ARGS the gradle build arguments to run.
ENV-VARS the environment variables to use when running gradle."

  (let* ((run-directory (gradle-run--get-root))
         (saved-default-directory default-directory)
         (gradlew-cmd (f-join run-directory gradle-run-gradlew-command)))
    (cd run-directory)
    (compile (concat env-vars " " gradlew-cmd " " build-args))
    (cd saved-default-directory)))

(defvar gradle-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g b") 'gradle-run-build)
    (define-key map (kbd "C-c C-g r") 'gradle-run)
    map)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-run-mode
  "Emacs minor mode for integrating Gradle into compile.
Run gradle tasks from any buffer, scanning up to the gradle project root directory to run tasks."
  :lighter " Gradle"
  :keymap 'gradle-run-mode-map
  :global t)

(provide 'gradle-run-mode)
;;; gradle-run-mode.el ends here
