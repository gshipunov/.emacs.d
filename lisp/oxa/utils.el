;;; Package management
(require 'package)

(defvar oxa/package-archive-refreshed nil)
(defun oxa/refresh-archive ()
    (when (not oxa/package-archive-refreshed)
      (package-refresh-contents)
      (setq oxa/package-archive-refreshed t)))

;; ensure package
(defun oxa/insure (package)
  (when (not (package-installed-p package))
    (oxa/refresh-archive)
    (package-install package)))

;; Macro to simplify setting mode-local vars
(defmacro oxa/hook (hook-name &rest body)
          `(add-hook ',hook-name '(lambda nil ,@body)))

(provide 'oxa/utils)
