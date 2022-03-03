;;; init.el --- emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; M-x 🦋
;;; Code:
;; bug in emacs<26.3
(if (version< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defconst oxa/using-native-comp (and (fboundp 'native-comp-available-p)
                                    (native-comp-available-p)))

(if oxa/using-native-comp
    (setq native-comp-deferred-compilation t
          native-comp-async-query-on-exit t
          native-comp-async-jobs-number 6
          native-comp-async-report-warnings-errors nil))

;; Change some settings based on where we are
(defvar oxa-workplace "home")

(if (string= oxa-workplace "work")
    ;; here we have our work proxy
    (message "we are at work"))

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

;; clean up the modeline
(straight-use-package 'diminish)
(require 'diminish)
(diminish 'auto-revert-mode)

(menu-bar-mode 1)
(tool-bar-mode -1)
(if (window-system)
    (toggle-scroll-bar -1))
(global-display-line-numbers-mode)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq visible-bell t)
(setq-default fill-column 80)

;; bigger frames
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; I'm the only cowboy on this mainframe
;; (setq create-lockfiles nil)

;; X is dead
(setq inhibit-x-resources t)

(straight-use-package 'nyan-mode)
(nyan-mode 1)

(straight-use-package 'which-key)
(which-key-mode)
(diminish 'which-key-mode)

;; use ibuffer instead of standard buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; my personal keymap
(define-prefix-command 'oxamap)
(global-set-key (kbd "C-z") 'oxamap)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

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

(defun oxa/graphical-setup ()
  "Setup bits for non-terminal frames."
  (if (window-system)
      (progn
        (use-package modus-themes
          :straight t
          :init
          (setq modus-themes-italic-constructs t
                modus-themes-bold-constructs t
                modus-themes-mixed-fonts nil
                modus-themes-subtle-line-numbers t)
          (modus-themes-load-themes)
          :config
          (modus-themes-load-operandi)
          :bind (:map oxamap ("\\" . modus-themes-toggle)))

        ;; use modern pdf-tools
        (straight-use-package 'pdf-tools)
        (pdf-loader-install)
        ;; theme pdf automagically
        (add-hook 'pdf-tools-enabled-hook 'pdf-view-themed-minor-mode))))

(defun oxa/frame-functions (frame)
  (with-selected-frame frame
        (oxa/graphical-setup)))

;; for stand-alone emacs
(oxa/graphical-setup)
;; for emacsclient
(add-hook 'after-make-frame-functions #'oxa/frame-functions)


;; let's delete a tab as a whole...
(setq backward-delete-char-untabify-method 'nil)

;; smarttabs!
(straight-use-package 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++)

(setq-default indent-tabs-mode 'nil)
(defun tabs-yay ()
  "Function to enable tab indentation in buffer."
  (setq indent-tabs-mode t))

(add-hook 'c-mode-hook 'tabs-yay)

;; highlight the parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; follow symlinks to version-controlled files
(setq vc-follow-symlinks t)

;; backup management
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; completion framework
(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (setq ido-file-extensions-order
        '(".org" ".scm" ".rkt" ".py" ".jl" ".txt" ".tex" ".bib"))
  :config
  (ido-mode t)
  (ido-everywhere t))

(use-package smex
  :straight t
  :commands (smex
             emex-major-mode-commands)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package ido-completing-read+
  :straight t
  :after ido
  :config (ido-ubiquitous-mode 1))

;; autocompletion by default
(straight-use-package 'company)
(company-mode 1)
(diminish 'company-mode)

;; better tree mode
(use-package neotree
  :straight t
  :bind (:map oxamap ("f" . 'neotree-toggle)))

;; CC mode default styles
(require 'cc-mode)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (c++-mode . "stroustrup")
                        (other . "stroustrup")))

(require 'calendar)
(setq calendar-week-start-day 1)

(use-package ace-window
  :straight t
  :defer t
  :bind (:map oxamap ("o" . ace-window)))

(use-package tex-site
  :defer t
  :straight auctex
  :init
  ;; reftex
  (require 'reftex)

  ;; basic config
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)

  ;; preview
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; completion for LaTeX
  (use-package company-auctex
    :straight t
    :config
    (company-auctex-init)))

(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (inferior-scheme-mode . rainbow-delimiters-mode)
         (geiser-mode . rainbow-delimiters-mode)
         (cc-mode . rainbow-delimiters-mode)))

(use-package org
  :straight t
  :commands (org-agenda
             org-capture)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c 1" . org-time-stamp-inactive))
  :init
  ;; we need indentation
  (setq org-startup-indented nil
        org-indent-mode-turns-on-hiding-stars nil
        org-hide-leading-stars nil
        org-startup-folded 'content)
  ;; default agenda files
  (setq org-agenda-files (cond ((string= oxa-workplace "home") '("~/org/"
                                                                 "~/Seafile/ORG/"))
                               ((string= oxa-workplace "work") '("D:/Seafile/ORG/"))))
  ;; default agenda view
  (setq org-agenda-start-day "-3d"
        org-agenda-span 13)
  ;; templates
  (setq org-capture-templates
        (cond ((string= oxa-workplace "home")
               '(("n" "note" entry
                  (file+headline "~/org/random.org" "Notes")
                  "** %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
                 ("t" "TODO" entry
                  (file+headline "~/org/random.org" "Tasks")
                  "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
                 ("w" "IFW Note" entry
                  (file+headline "~/Seafile/ORG/ifw.org" "ifw-notes")
                  "** %?\n%i\n%U\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                 ("j" "Journal" entry
                  (file+olp+datetree "~/org/log.org.gpg")
                  "**** %U %?\n")
                 ("b" "Bookmark" entry
                  (file+headline "~/org/bookmarks.org" "bookmarks-inbox")
                  "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n[[%x]]\n")))))
  ;; autosave advises for agenda and org-capture
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-capture-finalize :after 'org-save-all-org-buffers)
  (advice-add 'org-capture-refile :after 'org-save-all-org-buffers)

  ;; latex preview settings
  (setq org-preview-latex-image-directory "~/.emacs.d/org-latex-preview/") ; Hide all previews in one place

  ;; habits support
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)))

  ;; org-id - link by UUID
  (require 'org-id)
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id t)
  ;; abbrev expansion in org-mode
  (require 'org-tempo))

(use-package org-download
  :straight t
  :init (setq org-download-method 'directory
              org-download-image-dir "./static/org-download"
              org-download-heading-lvl 0))

(use-package magit
  :straight t
  :init (setq magit-completing-read-function 'magit-ido-completing-read)
  :bind (("C-x G" . magit-dispatch)
         ("C-x g" . magit-status)))

;; I positively cannot spell :D
(setq-default ispell-program-name (if (string= oxa-workplace "work")
                                      oxa-work-aspell
                                      "hunspell"))
(use-package flyspell
  :straight t
  :defer t
  :hook (('text-mode . (lambda () (flyspell-mode 1)))
         ('change-log-mode . (lambda () (flyspell-mode -1)))
         ('log-edit-mode . (lambda () (flyspell-mode -1)))
         ('prog-mode . (lambda () (flyspell-mode -1)))))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package yaml-mode
  :straight t)

(if (not (string= system-type "windows-nt"))
    (use-package vterm
      :straight t
      :bind (:map oxamap ("t" . vterm))
      :init
      (setq vterm-kill-buffer-on-exit t)))

;; checking
(straight-use-package 'flycheck)
(global-flycheck-mode)

;; language support and settings
(straight-use-package 'nix-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'editorconfig)

;; R stuff
(use-package ess
  :straight t
  :init
  (setq ess-use-ido t)
  (setq ess-use-flymake nil))

(use-package poly-R
  :straight t)

;; Julia
(use-package julia-mode
  :straight t)

(use-package julia-snail
  :straight t
  :after vterm
  :hook (julia-mode . julia-snail-mode))

;; scheming
;; (use-package racket-mode
;;   :straight t)
(use-package geiser-racket
  :straight t)

(use-package scheme
  :init (setq scheme-program-name "petite"))

(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

;; make lambda lambda :D
(add-hook 'scheme-mode-hook 'prettify-symbols-mode)
(add-hook 'inferior-scheme-mode-hook 'prettify-symbols-mode)
(add-hook 'geiser-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

;; python
(setq python-shell-interpreter "python")
(setq flycheck-python-pycompile-executable "python")

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-z l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :straight t :commands lsp-ui-mode)

;; snippets
(straight-use-package 'yasnippet)
(yas-global-mode t)
(diminish 'yas-minor-mode)

;; I use custom vars for local config, so let's put them to separate file, where
;; it's easier for git to ignore it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; start server
(server-start)

(provide 'init)
;;; init.el ends here
