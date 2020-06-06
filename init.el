;;; init.el --- emacs configuration
;;; Commentary:
;;; M-x 🦋
;;; Code:
;; bug in emacs<26.3
(if (version< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; package management with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package for sugar, but use straight.el under the hood
(straight-use-package 'use-package)
(require 'use-package)

;; essential config
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar 1)
(global-display-line-numbers-mode)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq visible-bell t)

;; magic in the world of idiotic defaults...
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; readline prevails
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Advice term to kill buffer after shell exits."
  (kill-buffer))

(defun my-whitespace-hook ()
  "Hook to show trailing whitespace and empty lines."
  (setq show-trailing-whitespace t
        indicate-empty-lines t))
(add-hook 'prog-mode-hook #'my-whitespace-hook)
(add-hook 'text-mode-hook #'my-whitespace-hook)

;; let's try to fix the pile of burning garbage that emacs calls a tab
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
  '((tab-mark 9 [187 9] [92 9])))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; let's delete a tab as a whole...
(setq backward-delete-char-untabify-method 'nil)

;; smarttabs!
(straight-use-package 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++)

;;helper functions to switch tab expansion on and off
(defun tabs-yay ()
  ;;(local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t))
(defun tabs-nay () (setq indent-tabs-mode nil))

;;wasteland of hooks regarding tabs behaviour
;;Remember how it "Just worked"™ in vim?
;;That's what you pay with for org mode
(add-hook 'prog-mode-hook 'tabs-yay)
(add-hook 'lisp-mode-hook 'tabs-nay)
(add-hook 'scheme-mode-hook 'tabs-nay)
(add-hook 'emacs-lisp-mode-hook 'tabs-nay)

;; highlight the parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;; follow symlinks to version-controlled files
(setq vc-follow-symlinks t)

;; mac-emacs spooky path shit
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize)))

;; backup management
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; color theme
(straight-use-package 'gruvbox-theme)
(straight-use-package 'leuven-theme)
(straight-use-package 'zenburn-theme)

(load-theme 'gruvbox-dark-hard t)

;; healthy people weeks are starting on Monday
(use-package calendar
  :init (setq calendar-week-start-day 1))

;; CC mode
(use-package cc-mode
  :init
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "linux")
                          (c++-mode . "stroustrup")
                          (other . "linux")))
  ;; extra c packages
  (use-package irony
    :straight t
    :hook (('c-mode . 'irony-mode)
           ('c++-mode . 'irony-mode)
           ('irony-mode . 'irony-cdb-autosetup-compile-options)))
  (use-package flycheck-irony
    :straight t
    :after (flycheck)
    :hook ('flycheck-mode . 'flycheck-irony-setup))
  (use-package company-irony
    :straight t
    :after (company)
    :config (add-to-list 'company-backends 'company-irony)))

(use-package tex-site
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :straight auctex
  :config
  (setq TeX-parse-self t))

(use-package company-auctex
  :straight t
  :config
  (company-auctex-init))

(use-package latex-preview-pane
  :straight t)

(use-package flycheck
  :straight t
  :config (global-flycheck-mode))

(use-package ivy
  :straight t
  :demand
  :bind (("\C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :init
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :config
  (counsel-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (cc-mode . rainbow-delimiters-mode)))

(use-package org
  :straight org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c 1" . org-time-stamp-inactive))
  :init
  ;; we need indentation
  (setq org-startup-indented t
        org-startup-folded 'content)
  ;; default agenda files
  (setq org-agenda-files '("~/nextcloud/org/"
                           "~/nextcloud/org-phone/"
                           "~/Seafile/ORG/"))
  ;; templates
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file+headline "~/nextcloud/org/random.org" "Tasks")
           "** TODO %?\n %i")
          ("T" "TODO+file" entry
           (file+headline "~/nextcloud/org/random.org" "Tasks")
           "** TODO %?\n %i\n %a")
          ("n" "note" entry
           (file+headline "~/nextcloud/org/random.org" "Notes")
           "** %U\n%?\n")
          ("i" "IFW TODO" entry
           (file+headline "~/Seafile/ORG/ifw.org" "Tasks")
           "** TODO %?\n %i \n%U")
          ("j" "Journal" entry
           (file+datetree "~/nextcloud/org/log.org.gpg")
           "**** %U %?\n")
          ("b" "Bookmark" entry
           (file "~/nextcloud/org/bookmarks.org")
           "* [[%x][%?]\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")))
  ;; autosave advices for agenda and org-capture
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-capture-finalize :after 'org-save-all-org-buffers)

  ;; babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp .t)
     (python . t)
     (C . t))))

(use-package org-tempo
  :after org)

(use-package magit
  :straight t
  :bind (("C-x C-g" . magit-dispatch)
         ("C-x g" . magit-status)))

(use-package gitattributes-mode
  :straight t)

(use-package gitconfig-mode
  :straight t)

(use-package gitignore-mode
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package gnuplot-mode
  :straight t)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))

(use-package elpy
  :straight t
  :after flycheck
  :hook
  (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (elpy-enable))

(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g f" . avy-goto-line)))

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :straight t
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :straight t
  :after projectile
  :demand
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (counsel-projectile-mode 1))

(use-package darkroom
  :straight t
  :init
  (setq darkroom-text-scale-increase 0))

(use-package company
  :straight t
  :hook ('prog-mode . 'company-mode))

;; I positively cannot spell :D
(use-package ispell
  :config
  (setq-default ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,de_DE,ru_RU")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE,ru_RU"))

(use-package flyspell
  :straight t
  :hook (('text-mode . (lambda () (flyspell-mode 1)))
         ('change-log-mode . (lambda () (flyspell-mode -1)))
         ('log-edit-mode . (lambda () (flyspell-mode -1)))
         ('prog-mode . 'flyspell-prog-mode)))

(use-package haskell-mode
  :straight t)

(use-package comment-tags
  :straight t
  :hook (('prog-mode . 'comment-tags-mode)
         ('tex-mode . 'comment-tags-mode))
  :init
  (setq comment-tags-require-colon 0))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package vterm
  :straight t
  :init
  (setq vterm-kill-buffer-on-exit t))

(use-package geiser
  :straight t
  :init
  (setq geiser-active-implementations '(racket)))

;; throw away all the list-of-custom-shit!
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
