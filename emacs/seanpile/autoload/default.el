;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +default/browse-project ()
  (interactive) (doom-project-browse (doom-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +default/browse-templates ()
  (interactive) (doom-project-browse +file-templates-dir))
;;;###autoload
(defun +default/find-in-templates ()
  (interactive) (doom-project-find-file +file-templates-dir))

;;;###autoload
(defun +default/browse-emacsd ()
  (interactive) (doom-project-browse doom-emacs-dir))
;;;###autoload
(defun +default/find-in-emacsd ()
  (interactive) (doom-project-find-file doom-emacs-dir))

;;;###autoload
(defun +default/browse-notes ()
  (interactive) (doom-project-browse +org-dir))
;;;###autoload
(defun +default/find-in-notes ()
  (interactive) (doom-project-find-file +org-dir))

;;;###autoload
(defun +default/browse-snippets ()
  (interactive) (doom-project-browse +snippets-dir))
;;;###autoload
(defun +default/find-in-snippets ()
  (interactive) (doom-project-find-file +snippets-dir))

;;;###autoload
(defun +default/find-in-config ()
  "Open a file somewhere in `doom-private-dir' via a fuzzy filename search."
  (interactive)
  (doom-project-find-file doom-private-dir))

;;;###autoload
(defun +default/browse-config ()
  "Browse the files in `doom-private-dir'."
  (interactive)
  (doom-project-browse doom-private-dir))

;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +default/man-or-woman ()
  "Invoke `man' if man is installed, otherwise use `woman'."
  (interactive)
  (call-interactively
   (if (executable-find "man")
       #'man
     #'woman)))

;; Universal 'compiling'; just look for a special build file that can be executed
;; from the project root.
;;;###autoload
(defun compile-from-project-root ()
  "Make the current build."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (not (file-exists-p "build.macosx"))
        (message "No build file present, ignoring...")

      (if (one-window-p t)
          (split-window-horizontally))
      (switch-to-buffer-other-window "*compilation*")
      (compile "./build.macosx")
      (other-window 1))))

;;;###autoload
(defun open-scratch-buffer-in-other-window ()
  "Open the scratch buffer in the other window (or create another window if only one exists)"
  (interactive)
  (when (one-window-p t)
    (split-window-horizontally)
    (other-window))
  (doom/open-scratch-buffer))

;;;###autoload
(defun open-git-status-in-other-window ()
  "Open `magit status`, focused in the current window"
  (interactive)
  (when (not (one-window-p t))
    (doom/window-zoom))
  (magit-status))
