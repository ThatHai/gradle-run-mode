(require 'compile)

(defcustom gradle-project-dir nil
  "String representing the root directory of the gradle project where to run gradlew."
  :group 'gradle
  :type 'string)

(defcustom gradle-read-build-args t
  "Specifiy whether to prompt for build arguments."
  :group 'gradle
  :type 'boolean)

(defcustom gradle-build-args ""
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defcustom gradle-read-env-vars t
  "Specifiy whether to prompt for environment variable(s) to use when running gradle."
  :group 'gradle
  :type 'boolean)

(defcustom gradle-env-vars ""
  "Build arguments for gradle."
  :group 'gradle
  :type 'string)

(defun gradle-run-directory ()
  (let ((run-dir (or gradle-project-dir
                     (locate-dominating-file default-directory "gradlew"))))
    (or run-dir
        (user-error "No gradle project to run"))))

(defun gradle-run ()
  "Run gradle command."
  (interactive)
  (if gradle-read-env-vars
      (setq gradle-env-vars
            (read-from-minibuffer "Env variables: " gradle-env-vars))
    (setq gradle-env-vars ""))

  (if gradle-read-build-args
      (setq gradle-build-args
            (read-from-minibuffer "Build args: " gradle-build-args))
    (setq gradle-build-args ""))

  (let ((gradle-build-directory (gradle-run-directory))
        (saved-default-directory default-directory))
    (cd gradle-build-directory)
    (compile (concat gradle-env-vars " " gradle-build-directory "gradlew " gradle-build-args))
    (cd saved-default-directory))
  )

(defvar gradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g b") 'gradle-run)
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
