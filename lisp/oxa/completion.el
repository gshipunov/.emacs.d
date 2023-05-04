(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'consult)

;; vertico uses history
(require 'savehist)
(savehist-mode)

;; orderless completion
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; completion framework
(require 'vertico)
(vertico-mode)

(setq enable-recursive-minibuffers t)

(keymap-set vertico-map "?" #'minibuffer-completion-help)
(keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
(keymap-set vertico-map "M-TAB" #'minibuffer-complete)

;; annotations for minibuffer
(require 'marginalia)
(marginalia-mode)

(keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

;; vertico-enabled replacements
(require 'consult)
(setq consult-narrow-key "<")

;; global binds
(global-set-key (kbd "C-c M-x") 'consult-mode-command)
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c k") 'consult-kmacro)
(global-set-key (kbd "C-c m") 'consult-man)
(global-set-key (kbd "C-c i") 'consult-info)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "C-x b") 'consult-buffer)                ;; orig. switch-to-buffer
(global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") 'consult-bookmark)            ;; orig. bookmark-jump
(global-set-key (kbd "C-x p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer



;; goto-map
(global-set-key (kbd "M-g e") 'consult-compile-error)
(global-set-key (kbd "M-g f") 'consult-flymake)               ;; Alternative: consult-flycheck
(global-set-key (kbd "M-g g") 'consult-goto-line)             ;; orig. goto-line
(global-set-key (kbd "M-g M-g") 'consult-goto-line)           ;; orig. goto-line
(global-set-key (kbd "M-g o") 'consult-outline)               ;; Alternative: consult-org-heading
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g k") 'consult-global-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)

;; search map
(global-set-key (kbd "M-s d") 'consult-find)
(global-set-key (kbd "M-s D") 'consult-locate)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s G") 'consult-git-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s k") 'consult-keep-lines)
(global-set-key (kbd "M-s u") 'consult-focus-lines)


;; finding files

;; minibuffer
(keymap-set minibuffer-local-map "M-s" #'consult-history)
(keymap-set minibuffer-local-map "M-r" #'consult-history)

(provide 'oxa/completion)
