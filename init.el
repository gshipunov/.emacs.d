(add-to-list 'load-path "~/.emacs.d/lisp/")
;;; basic bits
(require 'oxa/sane-defaults)
(require 'oxa/utils)
(require 'oxa/misc)

(require 'oxa/org)
(require 'oxa/latex)

;;; theme
(oxa/insure 'gruber-darker-theme)
(load-theme 'gruber-darker t)
(set-face-italic 'font-lock-comment-face t)
(set-face-italic 'font-lock-comment-delimiter-face nil)
(global-display-line-numbers-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(add-hook 'prog-mode-hook '(lambda () (whitespace-mode t)))

;;; package management
(require 'package)
(package-initialize)
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil)
;;; soruces
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
(oxa/insure 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; move text
(oxa/insure 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)

;;; ido/smex for completion
(oxa/insure 'smex)
(oxa/insure 'ido-completing-read+)
(require 'ido)
(require 'ido-completing-read+)
(setq ido-require-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)

;; direnv
(oxa/insure 'direnv)
(direnv-mode 1)

;; magit
(oxa/insure 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-file-dispatch)

;;; Language support
(oxa/insure 'nix-mode)
(setq nix-nixfmt-bin "nixpkgs-fmt")
(oxa/insure 'markdown-mode)
(oxa/insure 'yaml-mode)

(oxa/insure 'rust-mode)
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
(oxa/insure 'company)
(global-company-mode)

(custom-set-variables
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))

;; nya-nya-nya-nya-nya-nya
(oxa/insure 'nyan-mode)
(require 'nyan-mode)
(nyan-mode 1)

;; I use custom vars for local config, so let's put them to separate
;; file, where it's easier for git to ignore it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
