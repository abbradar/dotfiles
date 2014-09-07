(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'cl)
(require 'use-package)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(package-initialize)

(use-package evil
  :ensure evil
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

    ;; PageUp/PageDown/Home/End likes
    (define-key evil-motion-state-map (kbd "C-h") 'evil-first-non-blank)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-page-down)
    (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-page-up)
    (define-key evil-motion-state-map (kbd "C-l") 'evil-end-of-line)
    )
  )

; Remap caps to s-q.
; s-q is captured by xmonad so this should be an unused key.
(setf (gethash #xfe08 x-keysym-table) (aref (kbd "s-q") 0))
(global-set-key (kbd "s-q") 'toggle-input-method)

(defadvice toggle-input-method (before xkb-switch activate)
  (call-process "xkb-switch" nil nil nil "-s" "us")
  )

(use-package key-chord
  :ensure key-chord
  :config
  (progn
    ;; Exit insert mode by pressing j and then k quickly
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)
    )
  )

(use-package auto-complete
  :ensure auto-complete
  :config (global-auto-complete-mode t)
)

(use-package yasnippet
  :ensure yasnippet
  :config (yas-global-mode 1)
)

(use-package powerline
  :ensure powerline
  :config (powerline-default-theme)
)

(use-package linum-relative
  :ensure linum-relative
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
                         ))
                      )))
    )
  )

(use-package sql-indent
  :ensure sql-indent
  :defer t
  )

(use-package sql
  :defer t
  :config
  (progn
    (sql-set-product 'postgres)
    (add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
    (require 'sql-indent)
    )
  )

; python autocompletion
(use-package jedi
  :ensure jedi
  :commands jedi:setup
  :init
  (progn
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)
    )
  )

(use-package python
  :defer t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  )

;;; newline-and-indent on RET
(add-hook 'lisp-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(use-package ess-site
  :ensure ess
  :mode ("\\.R\\'" . R-mode)
  :interpreter ("R" . R-mode)
  )

(use-package auctex-latexmk
  :ensure auctex-latexmk
  :commands auctex-latexmk-setup
  )

(use-package auctex
  :ensure auctex
  :defer t
  :config
  (auctex-latexmk-setup)
  )

(use-package org
  :ensure org
  :defer t
  )

(use-package ghc
  :ensure ghc
  :commands ghc-init
)

(use-package haskell-mode
  :ensure haskell-mode
  :defer t
  :config (progn
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
            (add-hook 'haskell-mode-hook 'ghc-init)
            )
  )

; Ruby smart mode
(use-package ruby-mode
  :defer t
  :config
  (progn
    (setq rsense-home "/opt/rsense-0.3")
    (add-to-list 'load-path (concat rsense-home "/etc"))
    (use-package rsense
      :config
      (add-hook 'ruby-mode-hook
                (lambda ()
                  (add-to-list 'ac-sources 'ac-source-rsense-method)
                  (add-to-list 'ac-sources 'ac-source-rsense-constant)))
      )
    ))
