;; ================================================================
;; /Corfu/ the frontend for code intelligent completion
;; ================================================================
;; Last modified on 27 Dec 2024

;; /Orderless/: optional completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; /Corfu/: auto-completion frontend
(use-package corfu
  :custom
  ;; popup behaviors
  (corfu-auto              t)
  (corfu-auto-prefix       2)
  (corfu-preview-current nil)
  (corfu-auto-delay      0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  ;; quiting behaviors
  (corfu-quit-no-match    'separator)
  (corfu-quit-at-boundary 'separator)
  :custom-face (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-mode-map
              ("S-SPC" . corfu-insert-separator)) ; trigger to filter
  :hook ((after-init        . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  ;; free the RET key for lass intrusive behavior
  ;; (keymap-unset corfu-map "RET")

  ;; disable corfu auto-complete in python shell, use vertico instead
  (add-hook 'inferior-python-mode-hook
            (lambda () (corfu-mode -1)
              (when (and (eq *ac-system* 'vertico) nil)  ;fallback to vertico if t
                (setq completion-in-region-function #'consult-completion-in-region))))

  ;; disable corfu auto popup in text-mode
  (add-hook 'text-mode-hook
            (lambda () (setq-local corfu-auto nil)))
  )

;; Icons support
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; /Cape/: extensive backends for Capf
(use-package cape
  :init
  ;; prog-mode
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; text-mode
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  :bind (("C-<tab>" . completion-at-point))  ;alternative to "M-<tab>"
  :config
  (when (eq *lsp-client* 'eglot)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))


(provide 'init-corfu)
;; ================================================
;; init-corfu.el ends here
