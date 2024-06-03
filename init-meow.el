;;===Package managing (MELPA etc)
;;{{{ Set up package and use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)
;;}}}

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
(setq bookmark-default-file "~/.emacs.d/bookmarks") ;; save bookmarks to .emacs.bmk after each entry
(setq bookmark-save-flag 1)  

;;reduce the frequency of garbage collection by making it happen on
;;each 30MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)

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
;; (setq browse-url-browser-function 'eww-browse-url)

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
(setq user-full-name "Iurii Pon"
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


(setq global-undo-tree-mode t)


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
(insert-file-contents (concat "~/git/emacs-init/snipp/" fn)) 
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

;; Eat terminal (eshell improvement)
;; (use-package eat
;; :ensure t
;; :config

;; ;; For `eat-eshell-mode'.
;; (add-hook 'eshell-load-hook #'eat-eshell-mode)

;; ;; For `eat-eshell-visual-command-mode'.
;; (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
;;     )


;; Setup interactive shell to add aliases from .bashrc
(setq shell-file-name "bash")

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

;; possibility to create multiple vterm buffers with uniq names
;; if vterm buffer exist and opened -> press C-S-t -> new vterm buffer with uniq name will be opened
(defun new-vterm ()
  (interactive)
  (if (string= "*vterm*" (buffer-name))
      (rename-uniquely))
  (vterm)
  )

;; vterm terminal (work only with native-compiled Emacs)
;; sudo apt install cmake libvterm libtool-bin  libvterm-dev
(use-package vterm
    )

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
                    :height 150
                    :weight 'normal
                    :width 'normal)

;;nice pack of doom-themes
(use-package doom-themes
  :config
   ;; Corrects (and improves) org-mode's native fontification.
   (doom-themes-org-config)
   ;; (load-theme 'doom-zenburn 1)
   (load-theme 'doom-gruvbox 1)
   ;; (load-theme 'doom-wilmersdorf 1)
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

;;Add Linux PATH ENV variables to Emacs
(use-package exec-path-from-shell
  :ensure t
  :config
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
    )

(setq shell-command-switch "-ic")


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
;; Enable vertico
(use-package vertico
  :init
  :config
  (vertico-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 10)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t));; nice sort approach for completion


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))



;; consult: search, grep, project navigation 
(use-package consult
  :demand t
    )

;; Undo replacement
(use-package undo-fu
    )

;; ;;===Org-mode settings
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

;;Babel settings
(org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (shell . t)
       ))

;; Agenda
;; include all .org files from notes dir in agenda
(setq org-agenda-files '("~/ydisk/orgzly/agenda.org"))
;;Show next 10 days, not only this week
(setq org-agenda-span 10)
;;show agenda since today 
(setq org-agenda-start-on-weekday nil)

;;Any keywords can be used here
(setq org-todo-keywords
      '((sequence "TODO" "BACKLOG" "HOLD" "CANCELED" "|" "DONE" )))

(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "SpringGreen3" :weight bold))
        ("BACKLOG" . (:foreground "royal blue" :weight bold))
        ("HOLD" . (:foreground "gold3" :weight bold))
        ("CANCELED" . (:foreground "OrangeRed4" :weight bold))
        ))



;;save clocks history between sessions
;; clock-in C-c C-x C-i
;; clock-out C-c C-x C-o
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

;;Org-capture
;;Template for TODO
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/ws/org/notes/todo.org" "org-capture")
;;          "* TODO %?\n  %i\n  %a")
;;         ("j" "Journal" entry (file+datetree "~/ws/org/notes/todo.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")))

;; gpg encryption
;;enter pass for gpg files inside Emacs
(setq epa-pinentry-mode 'loopback)

;;Org-bullets
;;nice looking lists in org-mode with UTF-8 characters
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

)

;;===Tramp
;;add in /etc/ssh/ssh_config StrictHostKeyChecking no
;;connect to not default port C-x C-f /ssh:test@host#2222:/tmp
(use-package tramp
  :config
  (setq tramp-chunksize "500")
  (setq tramp-debug-buffer t)
  ;(setq tramp-verbose 10)
  (setq password-cache-expiry nil)
  (setq tramp-default-method "ssh")
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
    :defer t
    :ensure t
    :hook
        (python-mode . lsp)
        (go-mode . lsp)
    :commands lsp
    :config
   ;; performance improvments
        (setq lsp-idle-delay 0.500)
        (setq lsp-log-io nil) ; if set to true can cause a performance hit
        ;; turn off auto picking of project root folder
        (setq lsp-auto-guess-root  nil )
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
        ;; Python-specific
        ;; ignore some error codes
        ;; (setq lsp-pylsp-plugins-flake8-ignore ["D103" "E231" "E211"])
        (setq lsp-pylsp-plugins-flake8-ignore ["D103" "E231" "E211"])
        (setq lsp-pylsp-plugins-flake8-config "/home/i/.flake8rc")
        ;; (setq lsp-pylsp-plugins-flake8-enabled nil)
        (setq lsp-clients-python-settings '(:configurationSources ["pyflakes"]))
        (setq lsp-pyls-plugins-pylint-enabled nil)
)


;;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-ui
    :config
     ;; disable sideline fully
    ;; (setq lsp-ui-sideline-enable nil)
    ;; (setq lsp-ui-doc-show-with-cursor nil)
    ;; (setq lsp-ui-sideline-show-diagnostics nil)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-delay 0.500)
    (setq lsp-ui-sideline-update-mode t)
)


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
  ;; :mode (("\\.go\\" . go-mode))
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
  (require 'dap-dlv-go)
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
      (find-file-existing filename)))

;;===go-mode
(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  ;; :config
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
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

;;===Flycheck
(use-package flycheck
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
  :config
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))
    
)


;;===Markdown mode
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command '("pandoc" "--no-highlight")))


;;===Magit
(use-package magit)


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
(use-package ox-jira)

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

;; kubernetes.el
;; magit-like k8s client
;; (use-package kubernetes
;;     )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  :init
  (marginalia-mode)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; wgrep toggle
(use-package wgrep


)


(use-package pyvenv
:defer t
:diminish
:config
	;; (setenv "PY_VENV" <your-pyworkon-venvs-folder>)
	; Show python venv name in modeline
	;; (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
	(pyvenv-mode t))


(use-package cider
    :config
    (setq cider-show-error-buffer t               ;'only-in-repl
        cider-font-lock-dynamically nil         ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil
        cider-use-xref nil                      ; use lsp

        cider-repl-pop-to-buffer-on-connect nil ; REPL buffer shown at starup
        clojure-enable-kaocha-runner t          ; enable Kaocha test runner
        cider-repl-display-help-banner nil      ; disable help banner
        cider-print-fn 'puget                   ; pretty printing with sorted keys / set values
        cider-result-overlay-position 'at-point ; results shown right after expression
        cider-overlays-use-font-lock t
        cider-repl-buffer-size-limit 100        ; limit lines shown in REPL buffer
        cider-repl-history-size 42
        )
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))

(use-package clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        clojure-toplevel-inside-comment-form t  ;; evaluate expressions in comment as top level
))


;;========
;; PACKAGES LINE
;;Add new package above this line, Keyboard config must be last to download to override previous stuff
;;========

;;========Keybindings
;;Echo commands I haven’t finished quicker than the default of 1 second:
(setq echo-keystrokes 0.4)

;; Meow
(use-package meow
    :config
    (setq meow-use-clipboard t)
    (setf meow-expand-hint-remove-delay 0)
    
)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . beginning-of-line)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("A" . meow-to-block)
   '("a" . meow-append)
   '("o" . meow-open-below)
   '("O" . meow-block)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("s" . meow-kill)
   '("x" . meow-visual-line-expand)
   '("X" . meow-line-expand)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("v" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("p" . meow-yank)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . consult-line)
   '("<escape>" . meow-cancel-selection)))

(meow-setup)
(meow-global-mode 1)

;; override vterm keybindings to use it as ordinary buffer (copy,move e.t.c)
(with-eval-after-load 'meow
  (push '(vterm-mode . insert) meow-mode-state-list)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (add-hook 'meow-insert-enter-hook
                        (lambda () (vterm-copy-mode -1))
                        nil t)
              (add-hook 'meow-insert-exit-hook
                        (lambda () (vterm-copy-mode 1))
                        nil t)))
  (add-hook 'dired-mode-hook
              (lambda ()
                (add-hook 'meow-insert-enter-hook
                          (lambda () (vterm-copy-mode -1))
                          nil t)
                (add-hook 'meow-insert-exit-hook
                          (lambda () (vterm-copy-mode 1))
                          nil t)))


  )
;;meow in dired-mode 
;;  (push '(dired-mode . insert) meow-mode-state-list))

;; revert any buffer by key
(defun buffer-force-revert (&optional args)
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or args (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
        (error "The buffer has been modified"))

)


;; Custom keyboard mode to my personal keybindings
;; in above custom minor mode
(define-minor-mode mykbd-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-kbd-keys")
(mykbd-minor-mode 1)(defvar mykbd-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f3>") 'magit-branch-checkout)
    (define-key map (kbd "<f4>") 'magit-status)
    (define-key map (kbd "<f8>") 'magit-discard)
    (define-key map (kbd "<f5>") 'buffer-force-revert)
    (define-key map (kbd "<f6>") 'dired)
    (define-key map (kbd "<f7>") 'consult-ripgrep)
    (define-key map (kbd "<f11>") 'snipp) ;; custom snippet func
    (define-key map (kbd "<f12>") 'async-shell-command)
    (define-key map (kbd "<C-up>") 'shrink-window)
    (define-key map (kbd "<C-down>") 'enlarge-window)
    (define-key map (kbd "<C-left>") 'shrink-window-horizontally)
    (define-key map (kbd "<C-right>") 'enlarge-window-horizontally)
    (define-key map (kbd "C-c f") 'consult-find)
    (define-key map (kbd "C-S-t") 'new-vterm)
    (define-key map (kbd "C-M-5") 'query-replace-regexp)
    (define-key map (kbd "M-k") 'kill-buffer)
    (define-key map (kbd "M-o") 'next-window-any-frame)
    (define-key map (kbd "M-l") 'switch-to-buffer)
    (define-key map (kbd "M-y") 'consult-yank-from-kill-ring)
    (define-key map (kbd "M-]") 'vterm-command)
    (define-key dired-mode-map (kbd "/") 'consult-line)
    ;;;;;
    map)
  "mykbd-minor-mode keymap.")

;; initiate above custom minor mode
(define-minor-mode mykbd-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-kbd-keys")
(mykbd-minor-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(meow cider pyvenv wgrep corfu marginalia web-mode ox-jira magit flycheck-yamllint flycheck yaml-mode go-mode dap-mode lsp-ui lsp-mode which-key company-shell org-bullets undo-fu consult orderless rainbow-delimiters exec-path-from-shell doom-themes vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
