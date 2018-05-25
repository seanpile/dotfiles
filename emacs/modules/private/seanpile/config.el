;;; modules/private/seanpile/config.el -*- lexical-binding: t; -*-

(if (featurep! +bindings) (load! +bindings))

;;
;; Config
;;

;; Font
(add-to-list 'default-frame-alist
	     '(font . "Knack Nerd Font-12"))

;; Delay updates to give Emacs a chance for other changes
;; Smoother Scrolling
(setq linum-delay t)
(setq scroll-conservatively 5)
(setq scroll-step 5)
(setq auto-window-vscroll nil)

;; Ignore minor modes
(def-package! rich-minority
	      :init
	      (setq rm-blacklist
		    (format "^ \\(%s\\)$"
			    (mapconcat
			      #'identity
			      '("~" "Projectile.*" "WK" "SP" "\\$" "ivy" "ElDoc" "ws" "WS" "Undo-Tree")
			      "\\|")))
	      :config
	      (rich-minority-mode 1))

;; Solarized Dark theme
(def-package! solarized-theme
	      :config
	      (load-theme 'solarized-dark t))

; Smarter modeline
(def-package! smart-mode-line
	      :after solarized-theme
	      :init
	      (setq sml/no-confirm-load-theme t
		    sml/shorten-directory t
		    sml/shorten-modes t
		    sml/name-width 40
		    sml/mode-width 'full)
	      :config
	      (sml/setup))

;; Disable parens / smartparents
(after! paren (show-paren-mode -1))
(after! smartparens (smartparens-global-mode -1))

;; Disable flycheck in go
(add-hook! go-mode
	   (flycheck-mode -1))

;; Ignore whitespace issues
(setq whitespace-style nil)

;; Disable whitespace mode
(remove-hook 'after-change-major-mode-hook 'doom|show-whitespace-maybe)

;; Better file navigation
(def-package! ranger
	      :init
	      (setq ranger-cleanup-eagerly t)
	      (setq ranger-dont-show-binary t)
	      (setq ranger-modify-header nil)
	      (setq ranger-footer-delay 2)
	      :config
	      (ranger-override-dired-mode t)
	      (map! :ne "-" #'deer))

(defun never-ever-split-a-window () nil)
(setq split-window-preferred-function 'never-ever-split-a-window)

;; Universal 'compiling'; just look for a special build file that can be executed
;; TODO: extract build file into variable
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

(when (featurep 'evil)
  (when (featurep! +evil-commands)
    (load! +evil-commands))

  (when (featurep! +bindings)
    (defvar +default-repeat-forward-key ";")
    (defvar +default-repeat-backward-key ",")

    (eval-when-compile
      (defmacro do-repeat! (command next-func prev-func)
	"Makes ; and , the universal repeat-keys in evil-mode. These keys can be
	customized by changing `+default-repeat-forward-key' and
	`+default-repeat-backward-key'."
	(let ((fn-sym (intern (format "+evil*repeat-%s" (doom-unquote command)))))
	  `(progn
	     (defun ,fn-sym (&rest _)
	       (define-key evil-motion-state-map +default-repeat-forward-key #',next-func)
	       (define-key evil-motion-state-map +default-repeat-backward-key #',prev-func))
	     (advice-add #',command :before #',fn-sym)))))

      ;; n/N
      (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
      (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
      (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
      (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

      ;; f/F/t/T/s/S
      (setq evil-snipe-repeat-keys nil
	    evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
      (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
      (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse)

      ;; */#
      (do-repeat! evil-visualstar/begin-search-forward
		  evil-ex-search-next evil-ex-search-previous)
      (do-repeat! evil-visualstar/begin-search-backward
		  evil-ex-search-previous evil-ex-search-next)))
