;; Will's Emacs Startup Script
;;
;; Add the following to your .emacs file and
;; make sure all the directory points to the
;; right place
;;
;; (add-to-list 'load-path "~/site-lisp")
;; (require 'startup)
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(add-to-list 'load-path "~/.emacs.d/site-lisp/symfony")

(defvar running-on-windows (memq system-type '(windows-nt cygwin)))
(defvar running-on-linux (not running-on-windows))
(defvar running-on-x (eq window-system 'x))
(defvar running-on-z (string-equal system-name "zaurus.chicago"))

;; make emacs use the clipboard
(if running-on-linux
    (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(if running-on-windows
    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(w32shell-cygwin-bin "C:\\tools\\cygwin\\bin"))
  )


(require 'color-theme)
(require 'psvn)
(require 'ruby-mode)
(require 'paren-glint)
(require 'mmm-mode)
(require 'mmm-auto)
(require 'lorem-ipsum)
(require 'rails)
(load "rails" t)
(require 'cc-mode)
(require 'flash-compile)
;;(require 'symfony)
(require 'w32-find-dired)


;;php jazz
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.php3?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>"
    )))
(autoload 'php-mode "php-mode" "PHP editing mode" t)
;;(add-to-list 'auto-mode-alist '("\\.php3?\\'" . sgml-html-mode))


;;general jazz
;;color theme
;;(color-theme-subtle-hacker)
(color-theme-high-contrast)

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "~/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "~/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Use firefox as our browser.
(setq browse-url-netscape-program "firefox")

;;; find file at point
(require 'ffap)
;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
(ffap-bindings)
;; C-u C-x C-f finds the file at point
(setq ffap-require-prefix t)
;; browse urls at point via w3m
(setq ffap-url-fetcher 'w3m-browse-url)
;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'xsteve-ido-choose-from-recentf)
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key [(f12)] 'ibuffer)


(paren-glint-mode 1)
(defun try-complete-abbrev (old)
(if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\C-x\C-j" 'goto-line)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key [f8] 'mmm-parse-buffer)
(scroll-bar-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 1)
(setq inhibit-startup-message t)
(display-time)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rails-ws:default-server-type "mongrel")
 '(safe-local-variable-values (quote ((mmm-global-classes))))
 '(w32shell-cygwin-bin "C:\\tools\\cygwin\\bin"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;mmm-mode jazz
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)

;;(set-face-background 'mmm-output-submode-face  "Navy")
(set-face-background 'mmm-code-submode-face    "gray14")
;;(set-face-background 'mmm-comment-submode-face "DarkOliveGreen")
(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))
(add-hook 'html-mode-hook
          (lambda ()
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
;;(set-face-background 'mmm-default-submode-face nil);;Gets rid of background

;;js jazz
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))

;;lisp jazz
(load "elisp-config" t)


;;__________________________________________________________________________
;; SLIME Setup
(setq load-path (append (list "~/.emacs.d/site-lisp/slime")
                        load-path))
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; If you don't want eldoc-like behavior, comment out the following line
;;(slime-autodoc-mode)
;;__________________________________________________________________________

;;lisp-config needs SLIME
;;(load "lisp-config" t)

;;actionscript jazz
(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
;; Activate actionscript-mode for any files ending in .as
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
;; Load our actionscript-mode extensions.
(eval-after-load "actionscript-mode" '(load "as-config"))

;; python jazz
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(eval-after-load "python-mode" '(load "python-config" t))

;;csv jazz
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;css jazz
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(autoload 'css-mode "css-mode" nil t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))


;;ruby jazz
(show-paren-mode 1)
(require 'snippet)
(require 'find-recursive)
(add-to-list 'load-path "C:/tools/ruby/lib")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

;;Add DVC Requirements
;;(add-to-list 'load-path "~/site-lisp/dvc/lisp")
;;(add-to-list 'Info-default-directory-list "~/site-lisp/dvc/texinfo")
;;(require 'dvc-autoloads)
;;(load-file "~/site-lisp/dvc/dvc-load.el")

;;speedbar load
(add-to-list 'load-path "~/.emacs.d/site-lisp/speedbar-0.14beta4")
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(define-key-after (lookup-key global-map [menu-bar tools])
  [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])



;;(provide 'startup)


(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)


(autoload 'ispell-word "ispell"
"Check the spelling of word in buffer." t)
(global-set-key "\e$" 'ispell-word)
(autoload 'ispell-region "ispell"
"Check the spelling of region." t)
(autoload 'ispell-buffer "ispell"
"Check the spelling of buffer." t)
(autoload 'ispell-complete-word "ispell"
"Look up current word in dictionary and try to complete it." t)
(autoload 'ispell-change-dictionary "ispell"
"Change ispell dictionary." t)
(autoload 'ispell-message "ispell"
"Check spelling of mail message or news post.")
;still necessary
(setenv "TEMP" "c:/temp")
(setenv "TMP" "c:/temp")
; helpful
(setq text-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
(setq tex-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
(setq latex-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
; enable tex parser, also very helpful
(setq ispell-enable-tex-parser t)