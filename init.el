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
(toggle-scroll-bar -1)
(global-display-line-numbers-mode)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq visible-bell t)
(setq-default fill-column 80)

;; I'm the only cowboy on this mainframe
;; (setq create-lockfiles nil)

;; X is dead
(setq inhibit-x-resources t)

;; use modern pdf-tools
(straight-use-package 'pdf-tools)
(pdf-loader-install)

(straight-use-package 'nyan-mode)
(nyan-mode 1)

(straight-use-package 'direnv)
(direnv-mode)

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

(straight-use-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t nil)
(color-theme-sanityinc-tomorrow-night)

;; let's delete a tab as a whole...
(setq backward-delete-char-untabify-method 'nil)

;; smarttabs!
(straight-use-package 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++)

(setq-default indent-tabs-mode 'nil)
(defun tabs-yay ()
  "Function to enable tab indentation in buffer."
  (setq indent-tabs-mode t))

(add-hook 'cc-mode-hook 'tabs-yay)

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
(straight-use-package 'ivy)
(ivy-mode 1)
(diminish 'ivy-mode)
(straight-use-package 'counsel)
(counsel-mode 1)
(diminish 'counsel-mode)
(global-set-key (kbd "C-s") 'swiper)

;; autocompletion by default
(straight-use-package 'company)
(company-mode 1)
(diminish 'company-mode)

;; CC mode default styles
(require 'cc-mode)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

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
  (require 'reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :config
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-surce-correlate-start-server t)
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
         (racket-mode . rainbow-delimiters-mode)
         (cc-mode . rainbow-delimiters-mode)))

(use-package org
  :straight t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c 1" . org-time-stamp-inactive))
  :init
  ;; we need indentation
  (setq org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil
        org-hide-leading-stars nil
        org-startup-folded 'fold)
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
                 ("w" "IFW Note" entry
                  (file+headline "~/Seafile/ORG/ifw.org" "ifw-notes")
                  "** %?\n%i\n%U\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                 ("j" "Journal" entry
                  (file+olp+datetree "~/org/log.org.gpg")
                  "**** %U %?\n")
                 ("b" "Bookmark" entry
                  (file+headline "~/org/bookmarks.org" "bookmarks-inbox")
                  "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n[[%x]]\n")))
              ((string= oxa-workplace "work")
               '(("t" "IFW TODO" entry
                  (file+headline "D:/Seafile/ORG/ifw.org" "ifw-tasks")
                  "** TODO %?\n%i\n%U")
                 ("n" "IFW Note" entry
                  (file+headline "D:/Seafile/ORG/ifw.org" "ifw-notes")
                  "** %?\n%i\n%U\n")))))
  ;; autosave advises for agenda and org-capture
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-capture-finalize :after 'org-save-all-org-buffers)
  (advice-add 'org-capture-refile :after 'org-save-all-org-buffers)

  ;; latex preview settings
  (setq org-preview-latex-image-directory "~/.emacs.d/org-latex-preview/") ; Hide all previews in one place
  ;; org-id - link by UUID
  (require 'org-id)
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id t)
  :config
  ;; abbrev expansion in org-mode
  (require 'org-tempo))

(use-package org-roam
  :straight t
  :after org
  :bind (:map oxamap
              ("r t" . org-roam-dailies-goto-today)
              ("r f" . org-roam-node-find)
              ("r o" . org-roam-node-visit)
              ("r i" . org-roam-node-insert)
              ("r c" . org-roam-capture)
              ("r b" . org-roam-buffer-toggle))
  :hook ('after-init-hook . 'org-roam-mode)
  :init
  (setq org-roam-directory "~/roam"
        org-roam-v2-ack t
        org-roam-completion-system 'ivy
        org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-download
  :straight t
  :init (setq org-download-method 'directory
              org-download-image-dir "./static/org-download"
              org-download-heading-lvl 0))

(use-package magit
  :straight t
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
  (setq ess-use-flymake nil))

(use-package poly-R
  :straight t)

;; scheming
(straight-use-package 'racket-mode)
(setq scheme-program-name "petite")
;; make lambda lambda :D
(add-hook 'scheme-mode-hook 'prettify-symbols-mode)
(add-hook 'inferior-scheme-mode-hook 'prettify-symbols-mode)
(add-hook 'racket-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

;; python
(setq python-shell-interpreter "python")
(setq flycheck-python-pycompile-executable "python")

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-z l")
  :hook ((c-mode . lsp)
         (nix-mode . lsp)
         (python-mode . lsp)
         (LaTeX-mode . lsp)
         (TeX-mode . lsp)
         (ess-r-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
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

(provide 'init)
;;; init.el ends here
