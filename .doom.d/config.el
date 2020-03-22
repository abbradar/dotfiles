;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook! rust-mode
  (setq-default rust-indent-offset 2))

(add-hook! python-mode
  (setq-default python-shell-interpreter "python3"))

(setq display-line-numbers-type 'relative)
