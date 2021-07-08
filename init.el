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

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-display-line-numbers-mode)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq visible-bell t)

;; use ibuffer instead of standard buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; my personal keymap
(define-prefix-command 'oxamap)
(global-set-key (kbd "C-z") 'oxamap)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(set-face-italic 'font-lock-comment-face 1)
(set-face-italic 'font-lock-comment-delimiter-face nil)

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

(if window-system
    (progn
      (straight-use-package 'zenburn-theme)
      (load-theme 'zenburn t)))

;; let's try to fix the pile of burning garbage that emacs calls a
;; tab. If anyone reading actually knows why mixing tabs and spaces or
;; deleting the tab one space at a time is a good idea, please drop me
;; an email. I want to know.
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
      '((tab-mark 9 [187 9] [92 9])))
(set-face-inverse-video 'whitespace-tab nil)
(set-face-foreground 'whitespace-tab "#3c3c3c")
(set-face-background 'whitespace-tab nil)
(add-hook 'prog-mode-hook #'whitespace-mode)


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

;; mac-emacs spooky path shit
(when (eq system-type 'darwin)
  (progn
    (straight-use-package 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

;; backup management
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; CC mode default styles
(require 'cc-mode)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

;; completion by default - welcome to 2020
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
;;(setq ido-use-filename-at-point 't)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".org" ".scm" ".rkt" ".py" ".jl" ".txt" ".tex" ".bib"))

(straight-use-package 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'calendar)
(setq calendar-week-start-day 1)

(use-package ace-window
  :straight t
  :defer t
  :bind (:map oxamap ("o" . ace-window)))

(use-package tex-site
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :straight auctex
  :init
  (require 'reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :config
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
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
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c 1" . org-time-stamp-inactive))
  :init
  ;; we need indentation
  (setq ;org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil
        org-hide-leading-stars nil
        org-startup-folded 'content)
  ;; default agenda files
  (setq org-agenda-files (cond ((string= oxa-workplace "home") '("~/nextcloud/org/"
                                                                 "~/nextcloud/org/phone/"
                                                                 "~/Seafile/ORG/"))
                               ((string= oxa-workplace "work") '("D:/Seafile/ORG/"))))
  ;; default agenda view
  (setq org-agenda-start-day "-3d"
        org-agenda-span 13)
  ;; templates
  (setq org-capture-templates
        (cond ((string= oxa-workplace "home")
               '(("t" "TODO" entry
                  (file+headline "~/nextcloud/org/random.org" "Tasks")
                  "** TODO %?\n%i")
                 ("T" "TODO+file" entry
                  (file+headline "~/nextcloud/org/random.org" "Tasks")
                  "** TODO %?\n%i\n%a")
                 ("n" "note" entry
                  (file+headline "~/nextcloud/org/random.org" "Notes")
                  "** %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
                 ("W" "IFW TODO" entry
                  (file+headline "~/Seafile/ORG/ifw.org" "ifw-tasks")
                  "** TODO %?\n%i\n%U")
                 ("w" "IFW Note" entry
                  (file+headline "~/Seafile/ORG/ifw.org" "ifw-notes")
                  "** %?\n%i\n%U\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                 ("j" "Journal" entry
                  (file+olp+datetree "~/nextcloud/org/log.org.gpg")
                  "**** %U %?\n")
                 ("b" "Bookmark" entry
                  (file+headline "~/nextcloud/org/bookmarks.org" "bookmarks-inbox")
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

  ;; babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp .t)
     (python . t)
     (scheme . t)))
  ;; latex preview settings
  (add-to-list 'org-latex-packages-alist '("" "braket" t)) ; Dirac brakets
  (setq org-preview-latex-image-directory "~/.emacs.d/org-latex-preview/") ; Hide all previews in one place
  ;; org-id - link by UUID
  (require 'org-id)
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id t)
  :config
  ;; abbrev expansion in org-mode
  (require 'org-tempo))

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
                                      "aspell"))
(use-package flyspell
  :straight t
  :defer t
  :hook (('text-mode . (lambda () (flyspell-mode 1)))
         ('change-log-mode . (lambda () (flyspell-mode -1)))
         ('log-edit-mode . (lambda () (flyspell-mode -1)))
         ('prog-mode . (lambda () (flyspell-mode -1)))))

(use-package comment-tags
  :straight t
  :hook (('prog-mode . 'comment-tags-mode)
         ('markdown-mode . 'comment-tags-mode)
         ('tex-mode . 'comment-tags-mode)
         ('latex-mode . 'comment-tags-mode))
  :init
  (setq comment-tags-require-colon 0))


(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(if (not (string= system-type "windows-nt"))
    (use-package vterm
      :bind ("C-c t" . vterm)
      :init
      (setq vterm-kill-buffer-on-exit t)))

;; checking
(straight-use-package 'flycheck)
(global-flycheck-mode)

;; language support and settings
(straight-use-package 'nix-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'editorconfig)

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
(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
(setq flycheck-python-pycompile-executable "python")

;;; Interface
;; fill column
(setq-default fill-column 80)

(straight-use-package 'nyan-mode)
(nyan-mode 1)

(straight-use-package 'direnv)
(direnv-mode)

(straight-use-package 'which-key)
(which-key-mode)
;; use lsp if we have nativecomp - without it it's too slow :(
(if oxa/using-native-comp
    (progn
      (use-package lsp-mode
        :straight t
        :init
        (setq lsp-keymap-prefix "C-z l")
        :hook ((c-mode . lsp)
               (nix-mode . lsp)
               (python-mode . lsp)
               (LaTeX-mode . lsp)
               (TeX-mode . lsp)
               (lsp-mode . lsp-enable-which-key-integration))
        :commands lsp)
      (use-package lsp-ui :straight t :commands lsp-ui-mode)
      ))

;; I use custom vars for local config, so let's put them to separate file, where
;; it's easier for git to ignore it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
