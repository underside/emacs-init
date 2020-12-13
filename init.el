;Extra Repos and use-package installation

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

;; General settings

;; Bookmarks
;;
(define-key global-map [f9] 'list-bookmarks)
(define-key global-map [f10] 'bookmark-set)
;; define file to use
(setq bookmark-default-file "~/workspace/org/emacs/bookmarks")  
;; save bookmarks to .emacs.bmk after each entry
(setq bookmark-save-flag 1)  

;;Run emacs as server
;;to connect use command (can be used in X11 either)  emacsclient -create-frame --alternate-editor=""
;;(server-start)

(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)

;;Do not blink
(blink-cursor-mode -1)
(setq use-dialog-box nil)

;;dont use ring
(setq ring-bell-function 'ignore)
;;Move Buffer frames using Ctrl-<arrows>
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;confirmation before exit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Inhibit startup/splash screen, and initial-scratch message
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)
;; Message in scratch buffer
(setq initial-scratch-message ";;Hi, bro. What's up?")

;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")

;; package enable at startup?
(setq package-enable-at-startup nil)

;; Fix some error with packages
(setq package-check-signature nil)

;;User name and email
(setq user-full-name "Iurii Ponomarev"
      user-mail-address "underside@ya.ru")

;;Desktop autosave
(desktop-save-mode 1)
(savehist-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;.emacs file always load to register. Available with hotkeys: C-x r j e
(set-register ?e (cons 'file "~/.emacs.d/init.el"))

;; reduce the frequency of garbage collection by making it happen on
;; each 30MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 30000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Fringe settings
(fringe-mode '(2 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;;Electric-modes settings
;; closed open brackets automatically {},[],()
(electric-pair-mode    1) 

;;not indent previous line when press RET
;; (setq-default electric-indent-inhibit t) 

;; Line wrapping
;; Cursor moving by visual lines
(setq word-wrap t)
(global-visual-line-mode t)

;; Highlight search results
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Highlight matching brackets
;;Parantes-matchning--------------------------
;;Match parenthesis through highlighting rather than retarded jumps. Good!
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Delete selection and clipboard to linux and other copy/paste features
(delete-selection-mode t)
(setq select-enable-clipboard t) 

;; Newline when press Enter
;;(global-set-key (kbd "RET") 'newline)
;; Indentation settings
(setq-default
indent-tabs-mode nil
tab-width          4
c-basic-offset     4
standart-indent    4
lisp-body-indent   4
tab-stop-list (quote (4 8))
)

;; ediff
(setq-default ediff-forward-word-function 'forward-char)
;;Don't use strange separate control-window.
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;;Side by side comparison is easier than vertical split
;;(tob-bottom-stacked) window
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

;; Backups settings
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

;; Dired settings
;; allow remove non-empty dirs
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

;; Hide detailes in dired-mode (permissions etc) 
;;If you want see detailes press "("
(defun hide-details ()
  "To be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-details)

;; Themes,fonts,UI
;; enable pixelwise resizing frames
(setq frame-resize-pixelwise t)

;;Font
;;Set default font
;;font must be installed in OS
;;For Ubuntu: 'apt install fonts-hack'
(set-face-attribute 'default nil
                    ;; :family "DejaVu Sans Mono"
                    :family "Hack"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(use-package doom-themes
  :ensure t
  :config
   ;; (load-theme 'doom-zenburn t)
   (load-theme 'doom-one t)
   ;; (load-theme 'doom-spacegrey t)
   ;; (load-theme 'doom-nord t)
   ;; (load-theme 'doom-wilmersdorf t)
   ;; (load-theme 'doom-solarized-dark t)
   ;; Corrects (and improves) org-mode's native fontification.
   (doom-themes-org-config)
)

;;Bind-keys for using kbd
(use-package bind-key
  :ensure t
    )

;;Flash cursor area when it's jump widely
;; (use-package beacon
;;   :ensure t
;;   :config
;;     (beacon-mode 1)
;;     (setq beacon-blink-when-window-scrolls nil)
;;     (setq beacon-blink-when-point-moves nil)
;;     (setq beacon-size 20)                
;; )

;;Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
)

;; Modeline settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(setq display-time-24hr-format t) ;; 24-hour
(size-indication-mode          t) ;; file size in persents
;; show dirictory where file is modifying
(defun mode-line-buffer-file-parent-directory ()
  (when buffer-file-name
    (concat "[" (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))) "]")))
(setq-default mode-line-buffer-identification
      (cons (car mode-line-buffer-identification) '((:eval (mode-line-buffer-file-parent-directory)))))

;; (setq mode-line-format
;;           (list
;;            ;; current buffer name
;;            "%b "
;;            ;; major mode-name
;;            "%m "
;;            ;; value of current line number
;;            "%l:"
;;            ;; value of current line number
;;            "%c "
;; )) 

;;rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
)

;;Ivy
(use-package ivy
  :ensure t
  :config 
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  :bind 
    ("M-y" . counsel-yank-pop)
    ("<f6>" . ivy-resume)
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("<f1> f" . counsel-describe-function)
    ("<f1> v" . counsel-describe-variable)
    ("<f1> o" . counsel-describe-symbol)
    ("<f1> l" . counsel-find-library)
    ("<f2> i" . counsel-info-lookup-symbol)
    ("<f2> u" . counsel-unicode-char)
    ("C-x l" . counsel-locate)
    ("M-r" . counsel-minibuffer-history)
    (:map  ivy-minibuffer-map
      ("<left>" . delete-backward-char)
      ("<right>" . ivy-alt-done)
    )
)

(use-package counsel
  :ensure t
  :after ivy
  :bind
    ("M-y" . counsel-yank-pop)
)

(use-package swiper
  :ensure t
  :after ivy
  :bind
    ("C-s" . swiper)
)

;;yaml-mode
(use-package yaml-mode
  :ensure t
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
  ;;show ivy-switch-buffer by pressing "b" in execution mode
  (define-key evil-ex-map "b " 'ivy-switch-buffer)
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

;; only warn about deleting modified buffers.
  (setq ibuffer-expert t)


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
  ;; :ensure t
  :mode  ("\\.org\\'" . org-mode)
         ("\\org.gpg\\'" . org-mode)

  :config
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link)

  ;;encrypt in org-mode, cache save pass in session
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
)


(use-package ob-http
  :ensure t
)

(use-package ob-go
  :ensure t
)

;;Babel settings
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (go . t)
       (shell . t)

       ))

;;Agenda
;; ~/org is a symlink to /mnt/e/ydisk/org/notes
;; include all .org files from notes dir in agenda
(setq org-agenda-files '("~/workspace/org/notes/todo.org"))
;;Show next 10 days, not only this week
(setq org-agenda-span 10)
;;show agenda since today 
(setq org-agenda-start-on-weekday nil)

  (setq org-todo-keywords
        '((sequence "TODO" "HOLD" "|" "DONE" )))
;;save clocks history between sessions
;; clock-in C-c C-x C-i
;; clock-out C-c C-x C-o
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

;; Org-capture
;;Template for TODO
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/workspace/org/notes/todo.org" "org-capture")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/workspace/org/notes/todo.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; gpg encryption
;;enter pass for gpg files inside Emacs
(setq epa-pinentry-mode 'loopback)

;;Org-bullets
;;nice looking lists with UTF-8 characters
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

)

;;Tramp
;;add in /etc/ssh/ssh_config StrictHostKeyChecking no
;;connect to not default port C-x C-f /ssh:test@host#2222:/tmp
(use-package tramp
  :ensure t
  :config
  (setq tramp-chunksize "500")
  (setq tramp-debug-buffer t)
  ;(setq tramp-verbose 10)
  (setq password-cache-expiry nil)
  (setq tramp-default-method "ssh")
)

;;Company-mode
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'go-mode-hook (lambda ()
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)))
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.3) 
  (setq company-echo-delay 0.3)                          
  (setq company-minimum-prefix-length 1)
  (setq company-begin-commands '(self-insert-command))
)
;;lsp-mode (language server for different code lang)
;;install needed plugin first
;;GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (go-mode . lsp)
            (python-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :config
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
)

;; show tooltips 
(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

ivy integration
(use-package lsp-ivy
    :ensure t
    :commands
    lsp-ivy-workspace-symbol)


;;yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

;;Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (global-flycheck-mode 1)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-checker-error-threshold 2000)
)

;;Flycheck-yamllint
(use-package flycheck-yamllint
  :ensure t
  :config
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))
    
)

;;Projectile
(use-package projectile
  :ensure t
  :config
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :bind
  (:map global-map
        ("C-c p"       . projectile-find-file)
  )
)

;;Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         30
          treemacs-workspace-switch-cleanup      nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f8>"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;;Markdown mode
(use-package markdown-mode
  :ensure t
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
  (global-set-key (kbd "<f4>") 'magit-status)
  (global-set-key (kbd "<f5>") 'magit-branch-checkout)
)

(use-package evil-magit
  :ensure t
  :config
  (global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)
  (global-set-key (kbd "s-g") 'magit-status))

;;Jenkins
(use-package jenkinsfile-mode
  :ensure t
)

;; SPELL CHECKING
;; Spell checking requires an external command to be available. Install =aspell= on your Mac, then make it the default checker for Emacs' =ispell=. Note that personal dictionary is located at =~/.aspell.LANG.pws= by default.
(setq ispell-program-name "aspell")

;;Turn on flyspell for org-mode only
(add-hook 'org-mode-hook 'flyspell-mode)
;;keybinds
(global-set-key (kbd "S-\\") 'ispell-word)
(global-set-key (kbd "C-s-\\") 'flyspell-auto-correct-word)

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (setq yas-snippet-dirs
        '("~/workspace/org/emacs/snippets"))
  (yas-global-mode 1))

;;some prefab snippets
(use-package yasnippet-snippets
  :ensure t
)

;;Kubernetes
;;Magit-like porcelain to work with K8s
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;; Golang
;; add to below strings ~/.bashrc
;; export GOROOT=/usr/local/go
;; export GOPATH=$HOME/go/
;; export PATH=$PATH:$GOROOT/bin
;; export PATH=$PATH:$GOPATH/bin

;; go settings
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'yas-minor-mode)

(use-package go-mode
  :ensure t
  :defer t
  :mode (("\\.go\\'" . go-mode))
)


;;------DO NOT TOUCH CONFIG BELOW-----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" default)))
 '(ediff-split-window-function (quote split-window-horizontally) t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain) t)
 '(helm-completion-style (quote emacs))
 '(ispell-dictionary-alist
   (quote
    (("russian" "\\cy" "\\Cy" "[-]" nil
      ("-C" "-d" "ru-yeyo.multi" nil utf-8))
     ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
      ("-d" "en_GB.multi" "--add-extra-dicts=en_GB-variant_1.multi" nil iso-8859-1))
     (nil "[A-Za-z]" "[^A-Za-z]" "[']" nil
          ("-C" nil iso-8859-1)))) t)
 '(ispell-extra-args (quote ("--sug-mode=ultra" "--prefix=c:/mingw_mine")))
 '(ispell-program-name "aspell")
 '(package-selected-packages
   (quote
    (rainbow-delimiters diminish lsp-ivy lsp-ui deminish which-key dap-yaml dap-go dap-mode lsp-mode json-mode ob-go exec-path-from-shell multi-compile flymake-go flycheck-gometalinter treemacs-projectile treemacs-evil treemacs go-mode ob-http request restclient vterm htmlize beacon pomodoro org-pomodoro yasnippet-snippets dockerfile-mode ivy-prescient prescient jinja2-mode all-the-icons-ibuffer kubernetes-evil kubernetes adoc-mode helm-ag uniquify ansible ansible-vault jenkinsfile-mode eterm-256color evil-magit jdee groovy-mode popup-el emacs-async doom-modeline org-bullets yasnippet magit markdown-mode xterm-color flycheck-yamllint yaml-mode use-package helm flycheck evil-surround evil-matchit doom-themes company)))
 '(projectile-mode t nil (projectile))
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
