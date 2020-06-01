;;; gradle-mode.el --- Minor mode for running gradle build.

;;; Commentary:

;;; Code:
(require 'compile)

(defcustom gradle-mode-read-build-args t
  "Non nil to prompt for build arguments by gradle-run."
  :group 'gradle
  :type 'boolean)

(defcustom gradle-mode-build-args nil
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defcustom gradle-mode-env-vars nil
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defvar gradle-mode-gradlew-command (if (string= system-type "windows-nt") "gradlew.bat" "gradlew")
  "The gradle wrapper command.")

(defun gradle-mode--get-root ()
  "Return the root directory of the gradle project."
  (let ((root-dir (or (and (featurep 'projectile) (projectile-project-p))
                      (locate-dominating-file default-directory gradle-mode-gradlew-command))))
    (or root-dir
        (user-error "No gradle project to run"))))

(defun gradle-mode-set-env ()
  "Promt to read the environment variables setting for gradle-mode-env-vars."
  (interactive)
  (setq gradle-mode-env-vars
        (read-from-minibuffer "Env variables: " gradle-mode-env-vars)))

(defun gradle-mode-run ()
  "Run gradle command by interactively read arguments to run."
  (interactive)
  (when gradle-mode-read-build-args
      (setq gradle-mode-build-args
            (read-from-minibuffer "Build args: " gradle-mode-build-args)))
  (gradle-mode--run gradle-mode-build-args gradle-mode-env-vars))

(defun gradle-mode-build ()
  "Run gradle build task."
  (interactive)
  (gradle-mode--run "build" gradle-mode-env-vars))

(defun gradle-mode--run (build-args env-vars)
  "Run gradle with the specified build arguments and environment variables.

BUILD-ARGS the gradle build arguments to run.
ENV-VARS the environment variables to use when running gradle."

  (let* ((run-directory (gradle-mode--get-root))
         (saved-default-directory default-directory)
         (gradlew-cmd (f-join run-directory gradle-mode-gradlew-command)))
    (cd run-directory)
    (compile (concat env-vars " " gradlew-cmd " " build-args))
    (cd saved-default-directory)))

(defvar gradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g b") 'gradle-mode-build)
    (define-key map (kbd "C-c C-g r") 'gradle-mode-run)
    map)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-mode
  "Emacs minor mode for integrating Gradle into compile.
Run gradle tasks from any buffer, scanning up to nearest gradlew
directory to run tasks."
  :lighter " Gradle"
  :keymap 'gradle-mode-map
  :global t)

(provide 'gradle-mode)
;;; gradle-mode.el ends here
