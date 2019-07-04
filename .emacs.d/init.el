(require 'cl)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'use-package)
(require 'package)
;; Add NixOS packages
(add-to-list 'package-directory-list "/run/current-system/sw/share/emacs/site-lisp/elpa")
(package-initialize)

(use-package nix-sandbox
  :config
  (progn
    ;; Fix https://github.com/travisbhartwell/nix-emacs/issues/44
    (defun nix-shell-command (sandbox &rest args)
      "Assemble a command from ARGS that can be executed in the specified SANDBOX."
      (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox)
                                (mapconcat 'shell-quote-argument args " "))))))

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package evil
  :config
  (progn
    (evil-mode t)
    
    ;; eVIl leader options and mappings
    (define-key evil-normal-state-map (kbd ",ci") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map (kbd ",cc") 'evilnc-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map (kbd ",b") 'switch-to-buffer)

    ;; Window motions
    (define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
    (define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
    (define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
    (define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

    ;; Fix window creation
    (define-key evil-normal-state-map (kbd "^W n") 'split-window-below)
    
    ;; PageUp/PageDown/Home/End likes
    (define-key evil-motion-state-map (kbd "C-h") 'evil-first-non-blank)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-page-down)
    (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-page-up)
    (define-key evil-motion-state-map (kbd "C-l") 'evil-end-of-line)))

(use-package window-purpose
  :config
  (progn
    (purpose-mode)
    ;; Python
    (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
    (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
    ;; Clojure
    (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
    (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . cider))
    (purpose-compile-user-configuration)))

;; Remap caps to s-q.
;; s-q is captured by xmonad so this should be an unused key.
(setf (gethash #xfe08 x-keysym-table) (aref (kbd "s-q") 0))
(global-set-key (kbd "s-q") 'toggle-input-method)

(defadvice toggle-input-method (before xkb-switch activate)
  (call-process "xkb-switch" nil nil nil "-s" "us"))

(use-package key-chord
  :config
  (progn
    ;; Exit insert mode by pressing j and then k quickly
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))

(use-package company
  :config
  (global-company-mode))

(use-package projectile
  :config
  (progn
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(use-package ace-jump-mode
  :config (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package flycheck
  :config
  (progn
    (global-flycheck-mode)
    (flycheck-pos-tip-mode)
    (setq flycheck-command-wrapper-function
          (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
          flycheck-executable-find
          (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))))

(use-package magit
  :config (progn
            (global-set-key (kbd "C-x g") 'magit-status)
            (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)))

(use-package linum-relative
  :config
  (progn
    (global-linum-mode t)
    ;; Auto change between relative and absolute styles
    (add-hook 'post-command-hook
              (lambda ()
                (cond ((evil-insert-state-p)
                       (progn
                         (setq linum-format 'dynamic)
                         (linum-schedule)
                         ))
                      ((evil-normal-state-p)
                       (progn
                         (setq linum-format 'linum-relative)
                         (linum-schedule)
                         )))))))

(use-package sql-indent
  :defer t)

(use-package sql
  :defer t
  :config
  (progn
    (sql-set-product 'postgres)
    (add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
    (require 'sql-indent)))

(use-package python
  :defer t)

(use-package rust-mode
  :defer t
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'rust-mode-hook #'racer-mode)))

(use-package racer
  :defer t
  :config
  (progn
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package ess-site
  :mode ("\\.R\\'" . R-mode)
  :interpreter ("R" . R-mode))

(use-package auctex
  :defer t
  :config
  (add-hook 'tex-mode-hook
            (lambda ()
              (auctex-latexmk-setup)
              (auto-fill-mode)
              )))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup)

;; This adds ":NUMBERS:" property to exclude section from numbering.
(defun headline-numbering-filter (data backend info)
  "No numbering in headlines that have a property :numbers: no"
  (let* ((beg (next-property-change 0 data))
         (headline (if beg (get-text-property beg :parent data))))
    (if (and (eq backend 'latex)
         (string= (org-element-property :NUMBERS headline) "no"))
        (replace-regexp-in-string
         "\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)"
         "\\1*" data nil nil 1)
      data)))

(use-package org
  :config
  (progn
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (setq org-export-filter-headline-functions '(headline-numbering-filter))))

(use-package haskell-mode
  :mode "\\.chs\\'"
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    ;;(add-hook 'haskell-mode-hook 'intero-mode)
    ;;(evil-define-key 'normal haskell-mode-map (kbd "gs") 'intero-goto-definition)
    (setq haskell-process-wrapper-function
          (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))))

;;(use-package clojure-mode
;;  :config
;;  (progn
;;    (add-hook 'flycheck-mode-hook #'flycheck-clojure-setup)))

(use-package nix-mode
  :defer t)

(use-package intero
  :defer t)

;; Set default fallback font
(set-fontset-font "fontset-default" 'ucs "DejaVu Sans Mono")

;; Avoid closing all windows on ESC-ESC-ESC
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(provide 'init)
;;; init.el ends here
