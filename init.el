;;Extra Repos and use-package installation
(require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
(package-initialize)

;; Bootstrap `use-package' 
(unless (package-installed-p 'use-package)  
  (package-refresh-contents) 
  (package-install 'use-package)) 
(require 'use-package)

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
;;confirmation before exit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

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
(setq-default electric-indent-inhibit t) ;;not indent previous line when press RET

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

;;Indentation settings
(setq-default
indent-tabs-mode nil
tab-width          4
c-basic-offset     4
standart-indent    4
lisp-body-indent   4
tab-stop-list (quote (4 8))
)




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
(defun hide-details ()
  "To be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-details)

;;Shell,eshell,term
;;colours in terminal
;;xterm-color package, add colors to shell
;; (use-package xterm-color
;;   :config
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions)))

;;Themes,fonts,UI
;;enable pixelwise resizing frames
(setq frame-resize-pixelwise t)

;;Font
;;Set default font
;;font must be installed in OS
;;For Ubuntu: 'apt install fonts-hack'
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;Bind-keys for using kbd
(use-package bind-key)

;; Modeline settings
;; (line-number-mode t)
;; (column-number-mode t)
;; (size-indication-mode t)
;; (setq display-time-24hr-format t) ;; 24-hour
;; (size-indication-mode          t) ;; file size in persents


;; ;;Helm
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
         ("C-x r b" . helm-source-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         )
  :config
  (helm-mode 1)
  (define-key evil-ex-map "b " 'helm-mini)
  (define-key evil-ex-map "e" 'helm-find-files)
  (define-key evil-ex-map "g" 'helm-projectile-grep)
  (define-key evil-ex-map "f" 'helm-projectile-find-file)
)

;;yaml-mode
(use-package yaml-mode
  :ensure t
  )

(use-package doom-themes
   :config
   ;; (load-theme 'doom-one t)
   ;; (load-theme 'doom-spacegrey t)
   ;; (load-theme 'doom-zenburn t)
   ;; (load-theme 'doom-nord t)
   ;; (load-theme 'doom-wilmersdorf t)
   (load-theme 'doom-solarized-dark t)
   ;; Enable custom neotree theme (all-the-icons must be installed!)
   (doom-themes-neotree-config)
   ;; Corrects (and improves) org-mode's native fontification.
   (doom-themes-org-config)
)

;;doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110)
  )


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
;;Buffers
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
  (setq tramp-default-method "ssh")
)

;;yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

;;Flycheck
(use-package flycheck
  ;; :init (global-flycheck-mode)
  :config
  (global-flycheck-mode 1)
)

;;Flycheck-yamllint
(use-package flycheck-yamllint
  :ensure t
  ;; (progn
  ;;   (eval-after-load 'flycheck
  ;;     '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))
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
  (global-set-key (kbd "<f4>") 'magit-status))


(use-package evil-magit
  :ensure t
  :config
  (global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)
  (global-set-key (kbd "s-g") 'magit-status))

;;Groovy
(use-package groovy-mode
  :ensure t
)

;;Jenkins
;; (use-package jenkinsfile-mode
;;   :ensure t
;; )


;; SPELL CHECKING
;; Spell checking requires an external command to be available. Install =aspell= on your Mac, then make it the default checker for Emacs' =ispell=. Note that personal dictionary is located at =~/.aspell.LANG.pws= by default.
(setq ispell-program-name "aspell")
;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX. Spellcheck current word.
(add-hook 'text-mode-hook 'flyspell-mode)
(global-set-key (kbd "s-\\") 'ispell-word)
(global-set-key (kbd "C-s-\\") 'flyspell-auto-correct-word)

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.
(use-package yasnippet
  :defer t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;;which-key
;; (use-package which-key
;;   :ensure t
;;   :init
;;   :config
;;   ;; general improvements to which-key readability
;;   (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
;;   (which-key-setup-side-window-bottom)
;; )




;;------DO NOT TOUCH CONFIG BELOW-----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" default)))
 '(helm-completion-style (quote emacs))
 '(org-agenda-files (quote ("/mnt/org/notes/todo_personal.org")))
 '(package-selected-packages
   (quote
    (uniquify ansible ansible-vault jenkinsfile-mode eterm-256color evil-magit jdee groovy-mode popup-el emacs-async doom-modeline org-bullets yasnippet magit markdown-mode xterm-color flycheck-yamllint yaml-mode use-package helm flycheck evil-surround evil-matchit doom-themes company)))
 '(recentf-mode t)
 '(temp-buffer-resize-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dsilver ((t (:foreground "white smoke")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
