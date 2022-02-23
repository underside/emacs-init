;;===Package managing (MELPA etc)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;Manualy installed packages
;; without use-package
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;Bootstrap use-package
(unless (package-installed-p 'use-package)  
  (package-refresh-contents) 
  (package-install 'use-package)) 
(require 'use-package)

;; package enable at startup?
(setq package-enable-at-startup nil)

;; Fix some error with packages
(setq package-check-signature nil)

;;===General mixed settings
(windmove-default-keybindings)

;;lsp related settings from here https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;Bookmarks
;; (define-key global-map [f9] 'list-bookmarks)
;; (define-key global-map [f10] 'bookmark-set)
;; define file to use
(setq bookmark-default-file "~/.emacs.d/bookmarks")  
;; save bookmarks to .emacs.bmk after each entry
(setq bookmark-save-flag 1)  

;;reduce the frequency of garbage collection by making it happen on
;;each 30MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 30000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;Electric-modes settings
;; closed open brackets automatically {},[],()
(electric-pair-mode 1) 

;;Line wrapping
;;Cursor moving by visual lines
(setq word-wrap t)
(global-visual-line-mode t)

;;Highlight search results
(setq search-highlight        t)
(setq query-replace-highlight t)

;;Highlight matching brackets
;;Match parenthesis through highlighting rather than retarded jumps. Good!
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;;Delete selection and clipboard to linux and other copy/paste features
(delete-selection-mode t)
(setq select-enable-clipboard t) 

;; No region when it is not highlighted
(transient-mark-mode 1)

;;Backups settings
(setq make-backup-files nil;;do not make backup
      auto-save-files nil; do not create #autosave files
      create-lockfiles nil; do not create .# files
)

;; eww browser (Emacs)
;; use eww as default for URL
(setq browse-url-browser-function 'eww-browse-url)

;;===Buffers settings
;;Messages buffer: set max log size
(setq message-log-max 16384)


;; only warn about deleting modified buffers.
  (setq ibuffer-expert t)

;;===Interface and UI settings
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(setq use-dialog-box nil)

;;Do not blink
(blink-cursor-mode -1)

;;dont use ring
(setq ring-bell-function 'ignore)

;;confirmation before exit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;inhibit startup/splash screen, and initial-scratch message
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;;Message in scratch buffer
(setq initial-scratch-message ";;Hi, bro. What's up?")

;; Display the name of the current buffer in the title bar
(setq frame-title-format
      '(buffer-file-name "%b : %f" ; File buffer
        (dired-directory dired-directory ; Dired buffer
         (revert-buffer-function "%b" ; Buffer Menu
          ("%b - Dir: " default-directory))))) ; Plain buffer

;;User name and email
(setq user-full-name "Iurii Ponomarev"
      user-mail-address "underside@ya.ru")

;;Autosave-related 
(desktop-save-mode 1)
;; savehist
(setq savehist-additional-variables
      ;; also save my search entries
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq-default save-place t)

;; delete-auto-save-files
(setq delete-auto-save-files t)
(setq backup-directory-alist
      '(("." . "~/.emacs_backups")))

;; delete old backups silently
(setq delete-old-versions t)



;;enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Fringe settings
(fringe-mode '(2 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;;.emacs file always load to register. Available with hotkeys: C-x r j e
(set-register ?e (cons 'file "~/.emacs.d/init.el"))

;; smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; Newline when press Enter
;; Indentation settings
(setq-default
indent-tabs-mode nil
tab-width          4
c-basic-offset     4
standart-indent    4
lisp-body-indent   4
tab-stop-list (quote (4 8))
)

;;===Ediff settings
(setq-default ediff-forward-word-function 'forward-char)
;;Don't use strange separate control-window.
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;;Side by side comparison is easier than vertical split
;(tob-bottom-stacked) window
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

;;===Locale settings
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-input-method 'russian-computer)



;;===Dired settings
;;allow remove non-empty dirs
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
;;Hide detailes in dired-mode (permissions etc) 
;;If you want see detailes press "("
(defun hide-details ()
  "To be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'hide-details)

;;===Snippets
(defun snipp (fn) 
  "Load snippet from the file using filename."
(interactive "sSnippet:")
(insert-file-contents (concat "~/ws/git/emacs-init/snipp/" fn)) 
  (if nil (message "argument is nil")))
;; Abbrev table example
;; (define-abbrev-table 'global-abbrev-table '(
;;     ("afaict" "as far as I can tell" nil 1)
;; ))



;;===Terminal settings: ansi-term,vterm
;;===Config for Emacs without native-compile from source with ansi-term as main terminal
;; Term settings (if vterm is installed, this config is not needed)
;; open multiple terminals with index
;; (defun new-ansi-term ()
;;   (interactive)
;;   (if (string= "*ansi-term*" (buffer-name))
;;       (rename-uniquely))
;;   (ansi-term "/bin/bash")
;;   ;; (term-line-mode)   
;;   )
;; (global-set-key (kbd "C-S-t") 'new-ansi-term) ;; mappe sur C-T

;; vterm: Config with native-compile and vterm installed
;; use vterm instead of shell when run M-x shell-command (turn off by default)
;; set vterm as default shell
(setq-default shell-default-shell 'vterm)

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun vterm-command (command)
  "Execute string COMMAND in a new vterm.
Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.
Like `async-shell-command`, but run in a vterm for full terminal features.
The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.
When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " )))
      (read-shell-command "vterm command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)
    ))

;;possibility to create multiple vterm buffers with uniq names
(defun new-vterm ()
  (interactive)
  (if (string= "*vterm*" (buffer-name))
      (rename-uniquely))
  (vterm)
  )

;; vterm terminal (work only with native-compiled Emacs)
;; sudo apt install cmake libvterm libtool-bin  libvterm-dev
(use-package vterm
    :ensure t)

;;===Themes,fonts,UI
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

;;nice pack of doom-themes
(use-package doom-themes
  :ensure t
  :config
   ;; Corrects (and improves) org-mode's native fontification.
   (doom-themes-org-config)
   (load-theme 'doom-gruvbox 1)
   ;; (load-theme 'doom-wilmersdorf 1)
   ;; (load-theme 'doom-zenburn 1)
   ;; (load-theme 'doom-one 1)
   ;; (load-theme 'doom-plain 1)
   ;; (load-theme 'doom-solarized-dark 1)
   ;; (load-theme 'doom-solarized-light 1)
   ;; (load-theme 'doom-material 1)
   ;; (load-theme 'doom-dracula 1)
   ;; (load-theme 'doom-spacegrey 1)
   ;; (load-theme 'doom-nord 1)
)


;;===bind-key for using kbd
(use-package bind-key
  :ensure t
    )

;;Exec-path-from-shell
;; needed to use external tools from PATH
;; add ENVs from PATH
(setenv "PATH" "/usr/local/bin:$PATH" t)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/share")

;;===Modeline settings
;;Doom-modeline
;;Run M-x all-the-icons-install-fonts for this package
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
;;   :config
;;     (setq doom-modeline-height 8)
;;     ;;fonts
;;     (set-face-attribute 'mode-line nil :family "Hack" :height 110)
;;     (set-face-attribute 'mode-line-inactive nil :family "Hack" :height 110)
;;     ;; git info length
;;     (setq doom-modeline-vcs-max-length 12)
;;     ;;rm indent info
;;     (setq doom-modeline-indent-info nil)
;;     ;;remove encoding info
;;     (setq doom-modeline-buffer-encoding nil)


;;=== Custom Modeline
;;M font size and family
(set-face-attribute 'mode-line nil :family "Hack" :height 110)
(set-face-attribute 'mode-line-inactive nil :family "Hack" :height 110)
(line-number-mode t)
(column-number-mode 0)
(size-indication-mode t)
(setq display-time-24hr-format t) ;; 24-hour
(size-indication-mode          t) ;; file size in persents

;;list of minor modes needed to hide
(defvar hidden-minor-modes ; example, write your own list of hidden
  '(abbrev-mode            ; minor modes
    auto-fill-function
    smooth-scroll-mode
    mykbd-minor-mode
    evil-collection-unimpaired-mode
    projectile-mode
    which-key-mode
    auto-revert-mode
    eldoc-mode
    company-mode
    visual-line-mode
    ))

(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

;;rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
)

;;===Incremental narrowing
;;selectrum (ivy and helm replacement)
(use-package selectrum
  :ensure t
  :demand
  :config
    (selectrum-mode +1)

)
;; nice sort approach for completion
(use-package orderless
  :ensure t
  :config
    (setq orderless-skip-highlighting (lambda () selectrum-is-active))
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
    (setq completion-styles '(orderless))
)
;; consult - counsel and swiper replacement
(use-package consult
  :ensure t
  :demand t
    )
;;===Evil-mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  (setq evil-move-cursor-back nil)
  ;; this option needed for evil and evil-collection
  (setq evil-want-keybinding nil)

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
;; bind ':ls' command to 'ibuffer instead of 'list-buffers

 (define-key evil-ex-map "ls" 'ibuffer)
 ;; (define-key evil-normal-state-map (kbd "M-k") 'kill-buffer)
;;
 
;; define :b to open bufferlist search. Press : + b + space to start fuzzy search between opened buffers 
 (define-key evil-ex-map "b " 'ivy-switch-buffer)

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


;;Evil-mode plugin evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1)
)

;;plugin evil-surround
(use-package evil-surround
  :ensure t
  :after evil-mode
  :config
  (setq global-evil-surround-mode 1))

;;Evil and vterm
(defun evil-collection-vterm-escape-stay ()
"Go back to normal state but don't move
cursor backwards. Moving cursor backwards is the default vim behavior but it is
not appropriate in some cases like terminals."
(setq-local evil-move-cursor-back nil))

(add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

;;===Org-mode settings
(use-package org
  :mode  ("\\.org\\'" . org-mode)
         ("\\org.gpg\\'" . org-mode)
  :config
  ;; turn off confirmation before evaluate code block
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-block-startup t)

  ;;encrypt in org-mode, cache save pass in session
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

  ;; turn off 'org-indent-mode' by default
  (setq org-indent-mode nil)
  ;; fix error (underscore-to-subscript) when exporting
  (setq org-export-with-sub-superscripts nil)
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
       (http . t)

       ))

;;Agenda
;; ~/org is a symlink to /mnt/e/ydisk/org/notes
;; include all .org files from notes dir in agenda
(setq org-agenda-files '("~/ws/org/notes/todo.org"))
;;Show next 10 days, not only this week
(setq org-agenda-span 10)
;;show agenda since today 
(setq org-agenda-start-on-weekday nil)

;;Any keywords can be used here
  (setq org-todo-keywords
        '((sequence "TODO" "HOLD" "REVIEW" "|"  "REASSIGN" "CANCELED" "DONE" )))
;;save clocks history between sessions
;; clock-in C-c C-x C-i
;; clock-out C-c C-x C-o
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

;;Org-capture
;;Template for TODO
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/org/notes/todo.org" "org-capture")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/ws/org/notes/todo.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; gpg encryption
;;enter pass for gpg files inside Emacs
(setq epa-pinentry-mode 'loopback)

;;Org-bullets
;;nice looking lists in org-mode with UTF-8 characters
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

)

;;===Tramp
;;add in /etc/ssh/ssh_config StrictHostKeyChecking no
;;connect to not default port C-x C-f /ssh:test@host#2222:/tmp
;; (use-package tramp
;;   :ensure t
;;   :config
;;   (setq tramp-chunksize "500")
;;   (setq tramp-debug-buffer t)
;;   ;(setq tramp-verbose 10)
;;   (setq password-cache-expiry nil)
;;   (setq tramp-default-method "ssh")
;; )

;;===Company-mode
;; install shell-backend
(use-package company-shell
  :ensure t
)
(use-package company
  :ensure t
  :config
  ;; add company backends
  (add-to-list 'company-backends 'company-shell)
  (add-to-list 'company-backends 'company-python)

  ;; company-hooks for different modes
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'shell-mode-hook
            (lambda ()
                (set (make-local-variable 'company-backends) '(company-shell))))

  ;; base settings
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.3) 
  (setq company-echo-delay 0.3)                          
  (setq company-minimum-prefix-length 1)
  (setq company-begin-commands '(self-insert-command))
)
;;===lsp-mode
;;Golang -->  (see dt.org/Golang)
;;Python -->  pip install 'python-language-server[all]'

;;optional if you want which-key integration
(use-package which-key
    :ensure t
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.5)

    )

(use-package lsp-mode
    :ensure t
    :hook
        (sh-mode . lsp)
        (python-mode . lsp)
        (go-mode . lsp)
    :commands lsp
    :config
   ;; performance improvments
        (setq lsp-log-io nil) ; if set to true can cause a performance hit

        (setq lsp-enable-symbol-highlighting nil)


        ;; (setq lsp-keymap-prefix "C-c l")

        ;; Set up before-save hooks to format buffer and add/delete imports.
        ;; Make sure you don't have other gofmt/goimports hooks enabled.
        (defun lsp-go-install-save-hooks ()
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (add-hook 'before-save-hook #'lsp-organize-imports t t)
        )
        (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
        (add-hook 'go-mode-hook #'lsp-deferred)
        ;;gopls integration with lsp-mode
        (lsp-register-custom-settings
        '(("gopls.completeUnimported" t t)
        ("gopls.staticcheck" t t)))

        ;; turn off auto picking of project root folder
        (setq lsp-auto-guess-root  nil )

)


(use-package lsp-ui
    :ensure t
    :config
    ;; (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-delay 0)
    (setq lsp-ui-sideline-update-mode t)
)


;; lsp-ui-sideline-delay seconds to wait before showing sideline


;;===Dap-mode
;; For golang: Install lldb library first "apt install lldb" 
;; add new Unoptimized  "dap-debug-edit-template->edit template with new name-> eval buffer"
;; WARNING! Do not create launch.json manually
;; example for template
    ;; Eval Buffer with `M-x eval-buffer' to register the newly created template.
    ;; (dap-register-debug-template
    ;;   "Just Debug"
    ;;   (list :type "go"
    ;;         :request "launch"
    ;;         :name "Launch Unoptimized Debug Package"
    ;;         :mode "debug"
    ;;         :program "/home/<username>/golang/main.go"
    ;;         :buildFlags "-gcflags '-N -l'"
    ;;         :args nil
    ;;         :env nil
    ;;         :envFile nil))
(use-package dap-mode
  :ensure t
  ;; :mode (("\\.go\\'" . go-mode))
  :commands dap-debug
  :config
  (dap-mode 1)
  (dap-auto-configure-mode 1)     
  (setq dap-auto-configure-features '(sessions locals controls tooltip))     
  ;; The modes below are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  ;; for debug
  (setq dap-print-io t)

  ;;For golang
  (require 'dap-go)
  (dap-go-setup)
  ;; gdb
  (require 'dap-gdb-lldb)


  ;;workaround 
  (setq dap-launch-configuration-providers  '(dap-debug-template-configurations-provider))
  ;; register template 
  (dap-register-debug-template "Example Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :args ""
                                   :name "Run Configuration")))
  
  ;; For Python
  (require 'dap-python)

;; function that will add launch.json file in every root directory if it's not exist
;; run this function manually if debugger throw an error "....launch.json"
(defun dap-debug-create-or-edit-json-template ()
    "Edit the debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	  (default "~/ws/git/emacs-init/conf/default-launch.json"))
      (unless (file-exists-p filename)
	(copy-file default filename))
      (find-file-existing filename))))

;;===go-mode
(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :config
  ;; must if you use dap-mode

)

;; goimports hook to get packages before save
;; goimports must be installed
(defun my-go-mode-hook ()
      ;; prefer goimports, if present
      (if (executable-find "goimports")
        (setq gofmt-command "goimports"))

      ;; Format code when we save
      (add-hook 'before-save-hook 'gofmt-before-save)
 
      ;; esc-space to jump to definition
      (local-set-key (kbd "M-SPC") 'godef-jump)
      ;; esc-b to jump (b)ack
      (local-set-key (kbd "M-b") 'pop-tag-mark)
    )
    (add-hook 'go-mode-hook 'my-go-mode-hook)



;;===yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

;;===Flycheck
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

;;===Projectile
(use-package projectile
  :ensure t
  :config
    (projectile-mode +1)
  :bind
  (:map global-map
        ("C-c p"       . projectile-find-file))
)

;;===Treemacs
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
          treemacs-width                         38
          treemacs-workspace-switch-cleanup      nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (setq treemacs-text-scale -2)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("<f8>"   . treemacs)
        ))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)


;;===Markdown mode
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command '("pandoc" "--no-highlight")))


;;===Magit
(use-package magit
  :ensure t
)


;;===Spell check
;;Use hunspell to correct mistakes
;; (setq ispell-program-name "/usr/bin/hunspell"          
;;       ispell-dictionary   "en_US") ; Default dictionary to use
(when (executable-find "aspell")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
              "--lang=en_US"
              "--ignore=4")))
;;hunspell
(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-extra-args '("-d en_US")))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))

;; package used for export org files to Jira/Confluence markup 
; for export use M-x ox-jira-export-as-jira 
(use-package ox-jira
    :ensure t)

;;===Restclient API testing
(use-package restclient
    :ensure t)

(use-package ob-restclient
    :ensure t)

;;===Web-mode 
(use-package web-mode
    :config
    :mode
        (("\\.phtml\\'" . web-mode))
        (("\\.tpl\\.php\\'" . web-mode))
        (("\\.[agj]sp\\'" . web-mode))
        (("\\.as[cp]x\\'" . web-mode))
        (("\\.erb\\'" . web-mode))
        (("\\.mustache\\'" . web-mode))
        (("\\.djhtml\\'" . web-mode))
        (("\\.html?\\'" . web-mode))
     :config
        (setq web-mode-engines-alist
            '(("php"    . "\\.phtml\\'")
              ("blade"  . "\\.blade\\.")
              ("go"  . "\\.tpl\\.")
              )
        )
)

;;===Keybindings
;;Echo commands I haven’t finished quicker than the default of 1 second:
(setq echo-keystrokes 0.4)
;;big pack of evil-related libraries
(use-package evil-collection
  :ensure t
  :after evil
  :config
    (evil-collection-init)
)

;; escape quits
;; escape from any opened stuff like minibuffers etc
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

;;------------------------------------------------------------------------------------------------------------
;;Add new package above this line, Keyboard config must be last to download to override previous stuff
;;------------------------------------------------------------------------------------------------------------

;; Custom Minor-mode to override all keybindings in all modes
;; mykbd
(defvar mykbd-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f3>") 'magit-branch-checkout)
    (define-key map (kbd "<f4>") 'magit-status)
    (define-key map (kbd "<f6>") 'dired)
    (define-key map (kbd "<f7>") 'consult-grep)
    (define-key map (kbd "<f11>") 'snipp) ;; custom snippet func
    (define-key map (kbd "<f12>") 'async-shell-command)
    (define-key map (kbd "<C-up>") 'shrink-window)
    (define-key map (kbd "<C-down>") 'enlarge-window)
    (define-key map (kbd "<C-left>") 'shrink-window-horizontally)
    (define-key map (kbd "<C-right>") 'enlarge-window-horizontally)
    (define-key map (kbd "M-o") 'maximize-window)
    (define-key map (kbd "C-S-t") 'new-vterm)
    (define-key map (kbd "C-c p") 'projectile-find-file)
    (define-key org-mode-map (kbd "<normal-state> M-l") nil) ;;rm binding in org-mode
    (define-key map (kbd "M-l") 'switch-to-buffer)
    (define-key map (kbd "M-k") 'kill-buffer)
    (define-key map (kbd "C-s") 'consult-line)
    (define-key map (kbd "M-y") 'consult-yank-from-kill-ring)
    (define-key evil-normal-state-map (kbd "/") 'consult-line)
    ;; do not indent when press RET in org-mode
    (define-key org-mode-map (kbd "C-m") 'newline-and-indent)
    ;;----
    map)
  "mykbd-minor-mode keymap.")

;; initiate above custom minor mode
(define-minor-mode mykbd-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-kbd-keys")
(mykbd-minor-mode 1)

;; revert buffer using F5 key 
(global-set-key
  (kbd "<f5>")
  (lambda (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified"))))




;;------DO NOT TOUCH CONFIG BELOW-----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "e72f5955ec6d8585b8ddb2accc2a4cb78d28629483ef3dcfee00ef3745e2292f" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" default))
 '(ediff-split-window-function 'split-window-horizontally t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain t)
 '(ispell-dictionary-alist
   '(("russian" "\\cy" "\\Cy" "[-]" nil
      ("-C" "-d" "ru-yeyo.multi" nil utf-8))
     ("english" "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
      ("-d" "en_GB.multi" "--add-extra-dicts=en_GB-variant_1.multi" nil iso-8859-1))
     (nil "[A-Za-z]" "[^A-Za-z]" "[']" nil
          ("-C" nil iso-8859-1))) t)
 '(ispell-extra-args '("--sug-mode=ultra" "--prefix=c:/mingw_mine"))
 '(package-selected-packages
   '(dap-mode general evil-collection doom-modeline-now-playing doom-modeline web-mode auctex lsp-ui jq-mode ob-restclient confluence vterm ox-jira password-generator gitlab ag helm-flycheck rainbow-delimiters diminish deminish which-key lsp-mode json-mode ob-go exec-path-from-shell multi-compile flymake-go flycheck-gometalinter treemacs-projectile treemacs-evil treemacs go-mode ob-http request restclient htmlize beacon pomodoro org-pomodoro yasnippet-snippets dockerfile-mode jinja2-mode all-the-icons-ibuffer adoc-mode uniquify ansible ansible-vault jenkinsfile-mode eterm-256color evil-magit jdee popup-el emacs-async org-bullets yasnippet magit markdown-mode xterm-color flycheck-yamllint yaml-mode use-package flycheck evil-surround evil-matchit doom-themes company))
 '(projectile-mode t nil (projectile))
 '(recentf-mode t)
 '(temp-buffer-resize-mode t)
 '(warning-suppress-types '((emacs) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dsilver ((t (:foreground "white smoke")))))
