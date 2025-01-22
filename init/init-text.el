;; ===============================================================
;; General Plain Text Supports
;; ===============================================================
;; Last modified on 20 Feb 2020

(setq-default major-mode 'text-mode)

;; line wrapping
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ---------------------------------------------
;; English spelling checker
;; ---------------------------------------------
(use-package flyspell-correct)

(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell"
        ispell-dictionary   "british")
  :hook (text-mode . flyspell-mode)
  :config
  (defun zyue/toggle-dictionary ()
    "Toggle flyspell dictionary between the American and the British."
    (interactive)
    (if (string-equal ispell-dictionary "british")
        (setq ispell-dictionary "american")
      (setq ispell-dictionary "british"))
    (ispell-kill-ispell t)
    (message "%s" ispell-dictionary)))

(eval-after-load 'flyspell
  '(progn
     (define-key flyspell-mode-map (kbd "C-.")     'flyspell-correct-previous)
     ;; restore default keybindings
     (define-key flyspell-mode-map (kbd "M-<tab>") 'completion-at-point)))

;; ---------------------------------------------
;; Writing in distraction-free mode
;; ---------------------------------------------
(use-package olivetti
  :demand
  :hook ((olivetti-mode . (lambda () (auto-fill-mode -1)))
         (olivetti-mode . hide-mode-line-mode))
  :init (setq olivetti-body-width 75)  ;; .618
  :config
  (use-package hide-mode-line)
  (defalias 'writing-mode 'olivetti-mode))

;; ---------------------------------------------
;; Show key strokes & commands in demo of Emacs
;; ---------------------------------------------
(use-package command-log-mode)


(provide 'init-text)
;; ================================================
;; init-text.el ends here
