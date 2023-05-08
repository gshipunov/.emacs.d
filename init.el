(add-to-list 'load-path "~/.emacs.d/lisp/")

;; supress nativecomp warnings
(setq native-comp-async-report-warnings-errors 'quiet)

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
(straight-use-package 'gruvbox-theme)
(require 'gruvbox)
(setq gruvbox-bold-constructs t)
(load-theme 'gruvbox-dark-hard t)
(set-face-italic 'font-lock-comment-face t)
(set-face-italic 'font-lock-comment-delimiter-face nil)
(global-display-line-numbers-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(add-hook 'prog-mode-hook '(lambda () (whitespace-mode t)))

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
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (c++-mode . "stroustrup")
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
