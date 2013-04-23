(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

(if (not (daemonp))
    (reverse-input-method 'russian-computer)
  (defun rev-inp-m-init (f)
    (lexical-let ((frame f))
      (run-at-time nil nil
                   #'(lambda () (unless (string= (frame-parameter frame 'name) "F1")
                                  (reverse-input-method 'russian-computer)
                                  (remove-hook 'after-make-frame-functions #'rev-inp-m-init))))))
  (add-hook 'after-make-frame-functions #'rev-inp-m-init))
