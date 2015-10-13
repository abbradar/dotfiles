(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'cl)
(eval-when-compile
  (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
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

    ;; Fix window creation
    (define-key evil-normal-state-map (kbd "^W n") 'split-window-below)
    
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

(use-package flycheck
  :ensure flycheck
  :config (add-hook 'after-init-hook 'global-flycheck-mode)
  )

(use-package powerline
  :ensure powerline-evil
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
  )

(use-package python
  :defer t
  ;:config
  ;(add-hook 'python-mode-hook 'jedi:setup)
  )

;;; newline-and-indent on RET
(add-hook 'lisp-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(use-package ess-site
  :ensure ess
  :mode ("\\.R\\'" . R-mode)
  :interpreter ("R" . R-mode)
  )

(use-package auctex
  ;:ensure auctex
  :defer t
  :config
  (add-hook 'tex-mode-hook
            (lambda ()
              (auctex-latexmk-setup)
              (auto-fill-mode)
              ))
  )

(use-package auctex-latexmk
  :ensure auctex-latexmk
  :commands auctex-latexmk-setup
  )

; This adds ":NUMBERS:" property to exclude section from numbering.
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
  :ensure org
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (setq org-export-filter-headline-functions '(headline-numbering-filter))
    )
  )

;(use-package ghc
;  :ensure ghc
;  :commands ghc-init
;)

(use-package shm
  :ensure shm
  :commands structured-haskell-mode
  :config (progn
            (set-face-background 'shm-current-face "#eee8d5")
            (set-face-background 'shm-quarantine-face "lemonchiffon")
            )
)

(use-package haskell-mode
  :ensure haskell-mode
  :mode "\\.chs\\'"
  :config (progn
            ;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
            (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
            (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
            ; to be fixed for Nix
            ;(add-hook 'haskell-mode-hook 'ghc-init)
            )
  )

(use-package nix-mode
  :ensure nix-mode
  :mode "\\.nix\\'"
  )

; Ruby smart mode
(use-package rsense
  :defer t
  :init
  (progn
    (setq rsense-home "/opt/rsense-0.3")
    (add-to-list 'load-path (concat rsense-home "/etc"))
    )
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-rsense-method)
              (add-to-list 'ac-sources 'ac-source-rsense-constant)))
  )

(use-package erc
  :commands erc
  :config
  (add-hook 'erc-text-matched-hook 'erc-beep-on-match)
  )

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (require 'rsense nil t)
  )

(use-package hamlet-mode
  :ensure hamlet-mode
  :mode "\\.hamlet\\'"
  )

(use-package idris-mode
  :ensure idris-mode
  :mode "\\.idr\\'"
  )

(use-package agda-mode
  :mode ("\\.agda\\'" . agda2-mode)
  :mode ("\\.lagda\\'" . agda2-mode)
  :init (load-file (let ((coding-system-for-read 'utf-8)) (shell-command-to-string "agda-mode locate")))
  )

;; Set default fallback font
(set-fontset-font "fontset-default" 'ucs "DejaVu Sans Mono")
