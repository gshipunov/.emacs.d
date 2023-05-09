;; Macro to simplify setting mode-local vars
(defmacro oxa/hook (hook-name &rest body)
          `(add-hook ',hook-name #'(lambda nil ,@body)))

(provide 'oxa/utils)
