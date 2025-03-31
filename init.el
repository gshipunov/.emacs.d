(add-to-list 'load-path "~/.emacs.d/lisp/")

;; supress nativecomp warnings
(setq native-comp-async-report-warnings-errors 'silent)

;;; straight for package management
(require 'oxa/package-mgmt)

;;; basic bits
(require 'oxa/sane-defaults)
(require 'oxa/utils)
(require 'oxa/misc)
(require 'oxa/completion)

(require 'oxa/org)
(require 'oxa/latex)

;;; theme
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(load-theme 'modus-operandi)
(global-display-line-numbers-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(add-hook 'prog-mode-hook #'(lambda () (whitespace-mode t)))

;; match system theme
;; https://freerangebits.com/posts/2025/02/emacs-light-dark/
(defvar oxa:dark-theme 'modus-vivendi)
(defvar oxa:light-theme 'modus-operandi)
(defun oxa:theme-from-dbus (value)
  (load-theme (if (= 1 (car (flatten-list value)))
                  oxa:dark-theme
                oxa:light-theme)
              t))

(require 'dbus)

;; Set the current theme based on what the system theme is right now:
(dbus-call-method-asynchronously
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   #'oxa:theme-from-dbus
   "org.freedesktop.appearance"
   "color-scheme")

;; Register to be notified when the system theme changes:
(dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   (lambda (path var value)
     (when (and (string-equal path "org.freedesktop.appearance")
                (string-equal var "color-scheme"))
       (oxa:theme-from-dbus value))))

;;; personal framework bits
;; my personal keymap
(define-prefix-command 'oxamap)
(global-set-key (kbd "C-z") 'oxamap)
;; bind compile
(define-key 'oxamap (kbd "c") 'compile)

;;; tabs hooks
(defun tabs-yay ()
  "Function to enable tab indentation in buffer."
  (setq indent-tabs-mode t))

(add-hook 'c-mode-hook 'tabs-yay)

;;; expand region
(straight-use-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; move text
(straight-use-package 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)


;; direnv
(straight-use-package 'direnv)
(direnv-mode 1)

;; magit
(straight-use-package 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-file-dispatch)

;;; Language support
(straight-use-package 'nix-mode)
(setq nix-nixfmt-bin "nixpkgs-fmt")
(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)

(straight-use-package 'rust-mode)
(require 'rust-mode)
(oxa/hook rust-mode-hook
          (setq-local fill-column 100))

(require 'cc-mode)

;;; Copypasta of llvm/utils/emacs/emacs.el
;; LLVM coding style guidelines in emacs
;; Maintainer: LLVM Team, http://llvm.org/
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . llvm-lineup-statement)))))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
          (function
           (lambda nil
             (if (string-match "llvm" buffer-file-name)
                 (progn
                   (c-set-style "llvm.org"))))))

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "llvm.org")
                        (c++-mode . "llvm.org")
                        (other . "stroustrup")))

;; completion
(straight-use-package 'company)
(global-company-mode)

(custom-set-variables
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab tab-mark))))

;; nya-nya-nya-nya-nya-nya
(straight-use-package 'nyan-mode)
(require 'nyan-mode)
(nyan-mode 1)

;; I use custom vars for local config, so let's put them to separate
;; file, where it's easier for git to ignore it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
