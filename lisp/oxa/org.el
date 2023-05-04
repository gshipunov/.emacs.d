;;; Monday is the fist day of the week
(require 'calendar)
(setq calendar-week-start-day 1)

(straight-use-package 'org)
(straight-use-package 'org-roam)
(require 'org)
(require 'org-roam)

;; expose org functions
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c 1") 'org-time-stamp-inactive)

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
(require 'org-tempo)

;; we need indentation
(setq org-startup-indented nil
      org-indent-mode-turns-on-hiding-stars nil
      org-hide-leading-stars nil
      org-startup-folded 'content)
;; default agenda files
(setq org-agenda-files '("~/org/"))

;; default agenda view
(setq org-agenda-start-day "-3d"
      org-agenda-span 13)
;; templates
(setq org-capture-templates
      '(("n" "note" entry
         (file+headline "~/org/inbox.org" "Notes")
         "** %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
        ("t" "TODO" entry
         (file+headline "~/org/inbox.org" "Tasks")
         "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
        ("j" "Journal" entry
         (file+olp+datetree "~/org/log.org.gpg")
         "**** %U %?\n")
        ("b" "Bookmark" entry
         (file+headline "~/org/bookmarks.org" "bookmarks-inbox")
         "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n[[%x]]\n")))

;; TODO: roam config

(provide 'oxa/org)
