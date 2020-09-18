;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook! rust-mode
  (setq-default rust-indent-offset 2))

(add-hook! python-mode
  (setq-default python-shell-interpreter "python3"))

(add-hook! 'visual-fill-column-mode-hook #'visual-line-mode)

(add-hook! pollen-mode
  (visual-fill-column-mode))

(setq display-line-numbers-type 'relative)

(add-hook! haskell-mode
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(add-hook! fsharp-mode
  (setq inferior-fsharp-program "dotnet fsi --readline-"))
