(oxa/insure 'auctex)
(require 'tex-site)
(require 'reftex)

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)

(provide 'oxa/latex)
