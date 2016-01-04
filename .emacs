;; Will's Emacs Startup Script
;;
;; Add the following to your .emacs file and
;; make sure all the directory points to the
;; right place
;;


(add-to-list 'load-path "~/.emacs.d/site-lisp")
(let ((default-directory "~/.emacs.d/site-lisp/modules/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path
	     "~/.emacs.d/site-lisp/modules/emacs-color-theme-solarized")
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/modules/jdee/build/lisp")

(setq default-directory "~")

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))


(defvar running-on-windows (memq system-type '(windows-nt cygwin)))
(defvar running-on-darwin (string-equal system-type "darwin"))
(defvar running-on-linux (not running-on-windows))
(defvar running-on-x (eq window-system 'x))
(defvar running-on-z (string-equal system-name "zaurus.chicago"))

(if running-on-darwin
    (progn
      (setq ns-command-modifier 'meta)
      ;; Leave old Option key modifier as useful in terminal.
      ;;(setq ns-alternate-modifier 'none)
      ))

(require 'magit)
(require 'paren-glint)
(require 'mmm-mode)
(require 'mmm-auto)
(require 'lorem-ipsum)
(require 'cc-mode)
(require 'dired)
(require 'filecache)

(require 'uniquify)

;; Optionally load google elisp files.
(require 'google nil 'noerror)

;;general jazz
;;color theme
(load-theme 'solarized-dark t)
;;(enable-theme 'solarized-dark)

;; Go jazz
(require 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)


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
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

(require 'bash-completion)
(bash-completion-setup)

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

(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

;; Hippie expand configuration.
(load-file "~/.emacs.d/site-lisp/expand-config.el")

(paren-glint-mode 1)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\C-x\C-m" 'magit-status)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key [f8] 'mmm-parse-buffer)
(global-set-key [?\C-c ?i] 'ibuffer)


(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode nil))
(when (display-graphic-p)
    (tool-bar-mode -1)
 )
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(display-time)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#262626" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#626262"])
 '(background-color nil)
 '(background-mode dark)
 '(bash-completion-nospace t)
 '(before-save-hook (quote (gofmt-before-save)))
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(column-number-mode t)
 '(cursor-color nil)
 '(custom-enabled-themes nil)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color nil)
 '(java-mode-hook (quote (gcomplete-clang-completions-mode-func gcomplete-flymake-find-file (lambda nil (interactive) (column-marker-1 100)) google-set-java-style)))
 '(js2-bounce-indent-p t)
 '(nxml-attribute-indent 2)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(rails-ws:default-server-type "mongrel")
 '(safe-local-variable-values (quote ((mmm-global-classes))))
 '(send-mail-function (quote sendmail-send-it))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visible-bell nil))
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
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;python jazz
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;(require 'django-html-mode)

;; Python Hook to set indentation.
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; tail mode for log files.
(add-to-list 'auto-mode-alist '("\\.log" . auto-revert-tail-mode))
(add-to-list 'auto-mode-alist '("\\.STDERR\\'" . auto-revert-tail-mode))
(add-to-list 'auto-mode-alist '("\\.STDOUT\\'" . auto-revert-tail-mode))


(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;;__________________________________________________________________________
;; SLIME Setup
;;(setq load-path (append (list "~/.emacs.d/site-lisp/slime")
;;                        load-path))
;;(require 'slime)
;;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
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

;; Column markers
(require 'column-marker)
(add-hook 'js2-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 100)))
 (global-set-key [?\C-c ?m] 'column-marker-1)


;;ruby jazz
(show-paren-mode 1)
;;(require 'snippet)
;;(require 'find-recursive)
;;(add-to-list 'load-path "C:/tools/ruby/lib")
;;(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

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

;; Add exit confirmation.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Follow symlinks to the real file, w/o prompting us.
(setq vc-follow-symlinks t)


;; Add zip to emacs compress/uncompress exts.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; Needed to set this in order to bytecompile js2.el
(setq warning-suppress-types nil)

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
; helpful
(setq text-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
(setq tex-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
(setq latex-mode-hook '(lambda ()
(local-set-key "\M-\t" 'ispell-complete-word)))
; enable tex parser, also very helpful
(setq ispell-enable-tex-parser t)
(setq mac-command-key-is-meta t)

(defun find-grep-dired-do-search (dir regexp)
  "First perform `find-grep-dired', and wait for it to finish.
Then, using the same REGEXP as provided to `find-grep-dired',
perform `dired-do-search' on all files in the *Find* buffer."
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  (find-grep-dired dir regexp)
  (while (get-buffer-process (get-buffer "*Find*"))
    (sit-for 1))
  (with-current-buffer "*Find*"
    (dired-toggle-marks)
    (dired-do-search regexp)))

(define-key dired-mode-map (kbd "F") 'find-grep-dired-do-search)


;;(defalias 'xml-mode 'sgml-mode 
;;    "Use `sgml-mode' instead of nXML's `xml-mode'.")

;; Why is this failing??
;;(define-key dired-mode-map (kbd "F") 'find-grep-dired)

;; When switch windows, make the one you're switching to 80 wide.
;;(defadvice other-window (after other-window-now activate)
;;  (if (< (window-width) 80)
;;      (enlarge-window (- 81 (window-width)) t)
;;      (shrink-window (- (window-width) 81) t)
;;    ))

(defun gnome-navigate-using-nautilus (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process-shell-command "" nil "/usr/bin/nautilus" filename)))

(defun dired-gnome-navigate-using-nautilus ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (gnome-navigate-using-nautilus (dired-get-file-for-visit)))

(add-hook 'dired-mode-hook (lambda () (local-set-key "N" 'dired-gnome-navigate-using-nautilus)))

(defun html-escape-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

(setq calendar-latitude 40.67)
(setq calendar-longitude -73.94)
(setq calendar-location-name "New York, NY")

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;Yasnippet
(require 'yasnippet)

;; Java jazz
(require 'java-snippets)

;; Turn on yas mode for all buffers.
(yas-global-mode 1)
(define-key yas-minor-mode-map [C-tab] 'yas-expand)

(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (str)
	      (prog1 str
		(when (string-match comint-prompt-regexp str)
		  (cd (file-symlink-p
		       (format "/proc/%s/cwd" (process-id
					       (get-buffer-process
						(current-buffer)))))))))
	    nil t))

(add-hook 'shell-mode-hook 'track-shell-directory/procfs)

(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;;(defun google-java-save-buffer ()
;;  (interactive)
;;  (save-excursion
;;    (google-imports-organize-imports)
;;    (save-buffer)))

(if (string-match "google" (getenv "HOME"))
    (progn 
      (add-to-list 'package-archives
		   '("gelpa" . 
		     "http://internal-elpa.appspot.com/packages/"))
      (require 'google)
      (require 'p4-google)                ;; g4-annotate, improves find-file-at-point
      (require 'compilation-colorization) ;; colorizes output of (i)grep
      (require 'google3)                  ;; magically set paths for compiling google3 code
      (require 'google3-build)            ;; support for blaze builds
      (require 'csearch)                  ;; Search the whole Google code base.
      (require 'google-imports)           ;; Java google import magic.
      (require 'google-java)
      (require 'google3-ffap)
      (setq ffap-alist (append (google3-ffap-alist-additions) ffap-alist))
      (require 'ffap-java)
      (require 'google-prodaccess)
      (require 'google-cc-extras)
      (require 'gcomplete)
      (gcomplete-setup-flymake)
      ;; auto-complete if you want it as well
      (gcomplete-setup-for-auto-complete)
      (require 'google-jswat)
      (setq google-jdb-jswat-command "/google/src/files/head/depot/google3/devtools/editors/emacs/jswat/jswat-launcher")
      (remove-hook 'jdb-mode-hook 'google-jdb-fix-comint-prompt)
      (add-hook 'java-mode-hook
		'(lambda ()
		   (add-hook 'before-save-hook 'google-imports-organize-imports)))
      ;;eng/elisp/third_party/gnuemacs/jswat/jswat-launcher
      (require 'google-autogen)
      (require 'rotate-among-files)
      ;; Gtags key bindings
      (global-set-key (kbd "C-c C-g g g") 'gtags-feeling-lucky)
      (global-set-key (kbd "C-c C-g g u") 'gtags-show-tag-locations-under-point)
      (global-set-key (kbd "C-c C-g g n") 'google-next-tag)
      (global-set-key (kbd "C-c C-g g p") 'google-pop-tag)
      ;; Imports key bindings
      (global-set-key (kbd "C-c C-g p g") 'google-imports-grab-import)
      (global-set-key (kbd "C-c C-g p a") 'google-imports-add-grabbed-imports)
      (global-set-key (kbd "C-c C-g p o") 'google-imports-organize-imports)
      ;; Rotate between test/sut key bindings
      (global-set-key (kbd "C-c C-g r") 'google-rotate-among-files)
      (global-set-key (kbd "C-c C-g l")  'google-lint)

      (grok-init)
      ))

;;(mapc
;; (lambda (package)
;;   (unless (package-installed-p package)
;;     (package-install package)))
;; '(s f dash flycheck prodigy ...))
(package-initialize)


(server-start)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
