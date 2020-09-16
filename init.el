;;Extra Repos and use-package installation
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; keep the installed packages in .emacs.d
;;(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; update the package metadata is the local cache is missing
;(unless package-archive-contents
  ;(package-refresh-contents))

;;General settings
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode -1)
(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;Inhibit startup/splash screen, and initial-scratch message
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";;Hello, bro. What's up?")

;;=====Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;;package enable at startup?
(setq package-enable-at-startup nil)
;;Fix some error
(setq package-check-signature nil)

;;User settings
(setq user-full-name "Yury Ponomarev"
      user-mail-address "underside@ya.ru")

;;=====Desktop autosave
(desktop-save-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;.emacs file always load to register. Available with hotkeys: C-x r j e
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
;; (set-register ?t (cons 'file "/mnt/org/notes/todo.org"))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;Fringe settings
(fringe-mode '(0 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;;=====Electric-modes settings
(electric-pair-mode    1) ;; closed open brackets automatically {},[],()

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(setq display-time-24hr-format t) ;; 24-hour
(size-indication-mode          t) ;; file size in persents

;;Line wrapping
(setq word-wrap t)
(global-visual-line-mode t)

;; Highlight search results
(setq search-highlight        t)
(setq query-replace-highlight t)

;;highlight matching brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)



;; No region when it is not highlighted
(transient-mark-mode 1)

;;=====Delete selection and clipboard to linux and other copy/paste features
(delete-selection-mode t)
(setq x-select-enable-clipboard t) ;;Clipboard with Linux OS

;;newline when press Enter
(global-set-key (kbd "RET") 'newline)

;;Indentation settings(from habrahabr.ru)
(setq-default indent-tabs-mode nil)
(setq-default tab-width          2)
(setq-default c-basic-offset     2)
(setq-default standart-indent    2)
(setq-default lisp-body-indent   2)

;;Backups settings
(setq make-backup-files nil;;do not make backup
      auto-save-files nil; do not create #autosave files
      create-lockfiles nil; do not create .# files
)


;;Locale settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq default-input-method 'russian-computer)

;;Dired settings
;;allow remove non-empty dirs
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
;;hide detailes in dired like permissions
(defun hide-detailes ()
  "To be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-detailes)

;;Shell,eshell
;;colours in terminal
(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;;clear shell buffer by Ctr+L
(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;Themes,fonts,UI
;;enable pixelwise resizing frames
(setq frame-resize-pixelwise t)

;;Font
;; Set default font
;;font must be installed in OS apt install fonts-hack
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;xterm-color package, add colors to shell
(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

;;Helm
(use-package async
  :ensure t
  )

(use-package popup
  :ensure t
  )

(use-package helm
  :ensure t
  :bind (
         ("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([S-f10] . helm-recentf)
         ("C-x r b" . helm-source-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         )
  :config
  (helm-mode 1)
  ;; (global-set-key (kbd "M-x") #'helm-M-x)
  ;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key evil-ex-map "b " 'helm-mini)
  (define-key evil-ex-map "e" 'helm-find-files)
  (define-key evil-ex-map "g" 'helm-projectile-grep)
  (define-key evil-ex-map "f" 'helm-projectile-find-file)
)


;;
(use-package yaml-mode
  :ensure t
  )

;;Themes
;;Zenburn theme
(use-package zenburn-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'zenburn t)
  )

;; (use-package doom-themes
;;   :ensure t
;;   :load-path "themes"
;;   :config
;;   (load-theme 'doom-vibrant t)
;;   (doom-themes-neotree-config)
;;   )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 110)
  )

;;Bind-keys for using kbd
(use-package bind-key)

;;Company-mode
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

;;Evil
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  (setq evil-move-cursor-back nil)
  ;;Evil has the same problem as Vim when browsing with j/k long wrapped lines it jumps the entire “real” line ;;instead of the visual line. The solution is also easy:
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  ;;Don't move back cursor one postition when exiting INS mode
  (setq evil-move-cursor-back nil)
  ;;copy sentence - Y (standart Vim behavior)
  (setq evil-want-Y-yank-to-eol t)
  ;;C-k and C-j for page down/up
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
												    (interactive)
												    (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
												    (interactive)
												    (evil-scroll-down nil)))
  ;;:ls to ibuffer
  ;; bind ':ls' command to 'ibuffer instead of 'list-buffers
      (evil-ex-define-cmd "ls" 'ibuffer)
  ;;evil-mode as default for ibuffer
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

  ;;rebind C-r to U (so U is Redo)
  (with-eval-after-load 'evil-maps
	  (define-key evil-normal-state-map (kbd "U") 'undo-tree-redo))

  ;;more convinient undo
  (setq evil-want-fine-undo t)

 ;;change cursor color by evil state
 (setq evil-mode-line-format nil)
 (setq evil-normal-state-cursor '("gray" box))
 (setq evil-visual-state-cursor '("gray" box))
 (setq evil-insert-state-cursor '("green" bar))
 (setq evil-replace-state-cursor '("green" bar))
 (setq evil-operator-state-cursor '("red" hollow))

;; escape quits
(defun minibuffer-keyboard-quit ()
	(interactive)
	(if (and delete-selection-mode transient-mark-mode mark-active)
		(setq deactivate-mark  t)
		(when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
		(abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
)

;;Evil-mode plugin evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1)
)
;;Evil-mode plugin evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
)

;;Org-mode settings
(use-package org
  :ensure t
  :mode  ("\\.org\\'" . org-mode)
         ("\\org.gpg\\'" . org-mode)
  :config
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cl" 'org-store-link)
  ;;org-files default
  ;; (setq org-directory "~/.emacs.d/org")
  ;;encrypt in org-mode, cache save pass in session
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  )
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)))
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DELEGATED" "DONE")))

;;Org-bullets
;;nice looking lists with UTF-8 characters
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

)

;;gpg encryption
;;enter pass for gpg files inside Emacs
(setq epa-pinentry-mode 'loopback)

;;
;;Tramp
;;add in /etc/ssh/ssh_config StrictHostKeyChecking no
;;connect to not default port C-x C-f /ssh:test@host#2222:/tmp
(use-package tramp
  :config
  ;(setq tramp-chunksize "500")
  (setq tramp-debug-buffer t)
  ;(setq tramp-verbose 10)
  (setq password-cache-expiry nil)
)

;;yaml-mode
;; (use-package yaml-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; )

;;Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
)

;;Flycheck-yamllint
(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;;Neotree
(use-package neotree
  :ensure t
  :bind
  ("<f8>" . neotree-toggle)
  :config
  (setq neo-smart-open t)
)

;;Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
)


;;Markdown mode
(use-package markdown-mode
:defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("s-k" . 'markdown-insert-link)
              ("C-s-<down>" . 'markdown-narrow-to-subtree)
              ("C-s-<up>" . 'widen)
              ("s-O" . 'markdown-export-html-to-clipboard-full)
              ("M-s-O" . 'markdown-export-html-to-clipboard-no-1st-line))
  :init (setq markdown-command '("pandoc" "--no-highlight")))

;;Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)
  (global-set-key (kbd "s-g") 'magit-status))


(use-package evil-magit
  :ensure t
  :config
  (global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)
  (global-set-key (kbd "s-g") 'magit-status))


;; SPELL CHECKING
;; Spell checking requires an external command to be available. Install =aspell= on your Mac, then make it the default checker for Emacs' =ispell=. Note that personal dictionary is located at =~/.aspell.LANG.pws= by default.
(setq ispell-program-name "aspell")
;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX. Spellcheck current word.
(add-hook 'text-mode-hook 'flyspell-mode)
(global-set-key (kbd "s-\\") 'ispell-word)
(global-set-key (kbd "C-s-\\") 'flyspell-auto-correct-word)

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.
;; (use-package yasnippet
;;   :defer t
;;   :config
;;   (setq yas-snippet-dirs
;;         '("~/.emacs.d/snippets"))
;;   (yas-global-mode 1))


;;------DO NOT TOUCH CONFIG BELOW-----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (evil-magit jdee groovy-mode popup-el emacs-async doom-modeline org-bullets yasnippet magit markdown-mode xterm-color flycheck-yamllint zenburn-theme yaml-mode use-package helm flycheck evil-surround evil-matchit doom-themes company)))
 '(recentf-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
