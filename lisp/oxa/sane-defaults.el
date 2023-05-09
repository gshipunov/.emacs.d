;;; basic interface stuff
(menu-bar-mode 1)
(global-display-line-numbers-mode -1)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(setq-default indicate-buffer-boundaries 'left)
(setq auto-save-default nil)
(setq visible-bell t)
;; (setq-default fill-column 80)

;; TODO Proper graphical setup
(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (toggle-scroll-bar -1)
              (tool-bar-mode -1)))

;;; improvements to compilation buffer
(require 'compile)
(setq compilation-scroll-output t)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; override default annoyances
;; I'm the only cowboy on this mainframe
;; (setq create-lockfiles nil)
;; X is dead
(setq inhibit-x-resources t)
;; use ibuffer instead of standard buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; let's delete a tab as a whole...
(setq backward-delete-char-untabify-method 'nil)
;;; identation holywar contribution
(setq-default indent-tabs-mode 'nil)

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

;; default frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 107))

(provide 'oxa/sane-defaults)
