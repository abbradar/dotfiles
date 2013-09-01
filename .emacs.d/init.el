; CL extensions
(require 'cl)

; package.el
(package-initialize)

; Follow X layout switcher
; s-q is captured by xmonad, that's why
(setf (gethash #xfe08 x-keysym-table) (aref (kbd "s-q") 0))
(global-set-key (kbd "s-q") 'toggle-input-method)

;;; eVIl options
(require 'evil)

;;; eVIl leader options and mappings
(define-key evil-normal-state-map (kbd ",ci") 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map (kbd ",cc") 'evilnc-comment-or-uncomment-to-the-line)
(define-key evil-normal-state-map (kbd ",b") 'switch-to-buffer)

;;; Window motions
(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

;;; PageUp/PageDown/Home/End likes
(define-key evil-motion-state-map (kbd "C-h") 'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-end-of-line)

;;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
             (add-hook 'post-command-hook
                       (lambda ()
                         (let ((color (cond ((minibufferp) default-color)
                                            ((evil-insert-state-p) '("#556b2f" . "#ffffff"))
                                            ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                            ((evil-visual-state-p) '("#b8860b" . "#ffffff"))
                                            ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                            (t default-color))))
                           (set-face-background 'mode-line (car color))
                           (set-face-foreground 'mode-line (cdr color))))))

;;; little-words for evil (*lw)
(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))
(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

(require 'key-chord)
;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;;; auto-complete-mode
(eval-after-load "auto-complete"
  '(progn
     (global-auto-complete-mode t)
     ))

;;; yasnippet
(eval-after-load "yasnippet"
  '(yas-global-mode 1)
  )

;;; powerline
(eval-after-load "powerline"
  '(powerline-default-theme)
  )

;;; sql
(eval-after-load "sql"
  '(progn
     (sql-set-product 'postgres)
     (add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
     (load-library "sql-indent")
  ))

;;; jedi (python autocompletion)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;; newline-and-indent on RET
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;;; linum-relative
(require 'linum-relative)
(global-linum-mode t)

;;; ess
(require 'ess-site)

; auto change between relative and absolute styles
(add-hook 'post-command-hook
          (lambda ()
            (cond ((evil-insert-state-p) (progn
                                           (setq linum-format 'dynamic)
                                           (linum-schedule)
                                           ))
                  ((evil-normal-state-p) (progn
                                           (setq linum-format 'linum-relative)
                                           (linum-schedule)
                                           ))
                  )))

(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el") 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(c-basic-offset 2)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(evil-mode t)
 '(fill-column 80)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-archives (quote (("mepla" . "http://melpa.milkbox.net/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(python-indent-offset 2)
 '(rust-indent-unit 2 nil (rust-mode))
 '(standard-indent 2)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198 200)))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
