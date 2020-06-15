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
(global-set-key "\C-h" 'delete-backward-char)

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

;; let's try to fix the pile of burning garbage that emacs calls a
;; tab. If anyone reading actually knows why mixing tabs and spaces or
;; deleting the tab one space at a time is a good idea, please drop me
;; an email. I want to know.
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

;; wasteland of hooks regarding tabs behavior Remember how it "Just
;; worked"™ in vim? That's what you pay with for org mode
(add-hook 'prog-mode-hook 'tabs-yay)
(add-hook 'lisp-mode-hook 'tabs-nay)
(add-hook 'scheme-mode-hook 'tabs-nay)
(add-hook 'emacs-lisp-mode-hook 'tabs-nay)

;; time to throw out this "DocView" abomination: full featured pdf
;; viewer of antiquity, that emacs uses today!
(defun ensc/mailcap-mime-data-filter (filter)
  (mapcar (lambda(major)
            (append (list (car major))
                    (remove nil
                            (mapcar (lambda(minor)
                                      (when (funcall filter (car major) (car minor) (cdr minor))
                                        minor))
                                    (cdr major)))))
          mailcap-mime-data))

(defun ensc/no-pdf-doc-view-filter (major minor spec)
  (if (and (string= major "application")
           (string= minor "pdf")
           (member '(viewer . doc-view-mode) spec))
      nil
    t))

(eval-after-load 'mailcap
  '(progn
     (setq mailcap-mime-data
           (ensc/mailcap-mime-data-filter 'ensc/no-pdf-doc-view-filter))))

;; and this seems to make xdg-open work
(setq process-connection-type nil)

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

(load-theme 'leuven t)

;; CC mode default styles
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

;; swiper for search
(straight-use-package 'swiper)
(global-set-key "\C-s" 'swiper)
;; ivy for completion
(straight-use-package 'ivy)
(ivy-mode 1)
;; counsel for ivy-powered alternatives
(straight-use-package 'counsel)
(counsel-mode 1)

;; healthy people weeks are starting on Monday
(use-package calendar
  :init (setq calendar-week-start-day 1))

(use-package tex-site
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :straight auctex
  :config
  (setq TeX-parse-self t))

(use-package latex-preview-pane
  :straight t)

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
     (C . t)))
  :config
  ;; abbrev expansion in org-mode
  (require 'org-tempo))

(use-package magit
  :straight t
  :bind (("C-x C-g" . magit-dispatch)
         ("C-x g" . magit-status)))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))

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

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; throw away all the list-of-custom-shit!
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
