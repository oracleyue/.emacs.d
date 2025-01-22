;; ================================================================
;; Programming Environment for /Python/
;; ================================================================
;; Last modified on 06 Mar 2021

;; Install LSP langserver, like pyls, mspyls, pyright (default)

;; EDIT:
;; - shift selected blocks "C-c >", "C-c <"
;; - moving blocks/line: M-<left>, M-<right>, M-<up>, M-<down>
;;
;; SOURCE OVERVIEW: (more with "lsp-mode")
;; - counsel-semantic-or-imenu: "M-g i"
;;
;; DEBUG: (more with "dap-mode")
;; - "M-x pdb" then enter in minibuffer "pdb FILENAME.py"
;; - uncomment/insert "import pdb" "pdb.set_trace()" in python scripts;
;;   then evaluate buffer in iPython
;;
;; VENV:
;; - "M-x pyvenv-workon" to activate miniconda envs, necessary for LSP well-working

;; ------------------------------------------------
;; Python Environment
;; ------------------------------------------------
(use-package python
  :ensure nil
  :config
  ;; ---------------- Interpreter ----------------
  ;; If use virtualenv, controlled by /pyvenv/
  ;; (setq python-shell-interpreter "python"
  ;;       python-shell-interpreter-args "-i")

  ;; ------------------ Editing ------------------
  ;; tab/space detection
  (use-package dtrt-indent
    :diminish
    :hook (python-base-mode . dtrt-indent-mode))

  ;; indentation
  (defun zyue-py-indent-style ()
    (setq python-indent-offset 4
          tab-width 4
          python-indent-guess-indent-offset nil))
  (add-hook 'python-base-mode-hook #'zyue-py-indent-style)

  ;; ---------------- Code Intelligence ----------------
  ;; use LSP client (eglot, lsp-bridge)

  ;; ---------------- Virtual Environments ----------------
  ;; workflow: pyvenv activate -> revert-buffer ("s-u")
  ;; [trick]: "revert-buffer" triggers eglot reboot and modeline update of python version
  (use-package pyvenv
    :ensure t
    :bind (:map python-base-mode-map
                ("M-s C-a"  .  pyvenv-activate)  ;; venv at current folder
                ("M-s C-d"  .  pyvenv-deactivate)
                ("M-s C-e"  .  pyvenv-workon))   ;; select venv
    :config
    (setenv "WORKON_HOME" "~/miniconda/envs")
    (pyvenv-mode 1))

  ;; ---------------- Running Interface ----------------
  (defun python-shell-send-line (&optional beg end)
    (interactive)
    (let ((beg (cond (beg beg)
                     ((region-active-p)
                      (region-beginning))
                     (t (line-beginning-position))))
          (end (cond (end end)
                     ((region-active-p)
                      (copy-marker (region-end)))
                     (t (line-end-position)))))
      (python-shell-send-region beg end)))
  ;; add keybinding and menu item
  (dolist (map '(python-mode-map python-ts-mode-map))
    (define-key (symbol-value map) (kbd "C-c C-j") 'python-shell-send-line)
    (easy-menu-add-item python-ts-mode-map
                        '(menu-bar  "Python")
                        ["Eval line" python-shell-send-line :help "Eval line in inferior Python session"]
                        "Eval region"))

  ;; ---------------- Auto-completion ----------------
  ;; use LSP: lsp-mode ("init-lsp.el") or lsp-bridge ("init-lsp-bridge.el")

  ;; ---------------- Debugging ----------------
  ;; use built-in GUD debugger
  (define-advice pdb (:before (&optional arg) gud-query-cmdline)
    (interactive (list (gud-query-cmdline
                        'python (concat "-m pdb " (file-name-nondirectory
                                                   buffer-file-name))))))

  ;; use DAP via /dape/ mode for debugging (see "init-dap.el")

  ;; ---------------- Running script in shell ----------------
  (add-hook 'python-base-mode-hook
            (lambda () (set (make-local-variable 'compile-command)
                       (concat "python " buffer-file-name))))
  :bind (:map python-base-mode-map
              ("<f12>"  .  compile))

  ) ;; End of python-mode


(provide 'init-python)
;; ================================================
;; init-python.el ends here
