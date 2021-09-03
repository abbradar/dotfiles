;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq display-line-numbers-type 'relative)

(after! nix-sandbox
  (defun nix-find-sandbox (path)
    (if-let ((sandbox (locate-dominating-file path "shell.nix")))
        (concat (expand-file-name sandbox) "shell.nix"))))

(defun default-nix-wrapper (args)
  (if-let ((sandbox (locate-dominating-file default-directory "shell.nix")))
      (let ((abs-path (expand-file-name sandbox)))
        (if (file-exists-p (concat abs-path "flake.nix"))
            (append
             '("nix" "develop")
             (list (concat abs-path "#"))
             '("--command")
             args)
          (append
           '("nix-shell" "--run")
           (list
            (mapconcat 'shell-quote-argument args " ")
            (concat abs-path "shell.nix")))))
    args))

(add-hook! rust-mode
  (setq-default rust-indent-offset 2))

(add-hook! python-mode
  (setq-default python-shell-interpreter "python3"))

(after! irony
  (setq irony-server-install-prefix "/run/current-system/sw"))

(add-hook! pollen-mode
  (visual-fill-column-mode))

(add-hook! 'visual-fill-column-mode-hook #'visual-line-mode)

(add-hook! flycheck-mode
  (setq flycheck-command-wrapper-function 'default-nix-wrapper)
  (setq flycheck-executable-find (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd))))

(add-hook! haskell-mode
  (setq lsp-haskell-server-wrapper-function 'default-nix-wrapper)
  (setq haskell-process-wrapper-function 'default-nix-wrapper))

(add-hook! fsharp-mode
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

(use-package! cobol-mode
  :commands cobol-mode
  :mode ("\\.cob\\'"))

(add-hook! alchemist-mode
  (setq inferior-fsharp-program "dotnet fsi --readline-"))
