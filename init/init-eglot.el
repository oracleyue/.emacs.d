;; ================================================================
;; Emacs client for the Language Server Protocol
;; ================================================================
;; Last modified on 25 Dec 2024

(use-package eglot
  :ensure nil
  :hook (((python-mode python-ts-mode) . eglot-ensure)
         ;; (octave-mode . eglot-ensure)
         ((c-mode c-ts-mode)     . eglot-ensure)
         ((c++-mode c++-ts-mode) . eglot-ensure))
  :init (setq eglot-autoshutdown t)
  :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  :bind (:map prog-mode-map
              ;; M-./M-, for xref jump to def and back
              ;; C-M-i   for code completion
              ;; use "imenu"/"imenu-list" to list symbols
              ("M-s t" . eglot-find-typeDefinition)
              ("M-s d" . eglot-find-declaration)
              ("M-s i" . eglot-find-implementation)
              ("M-s ;" . eglot-rename)
              ("M-s f" . eglot-format)
              ("M-s a" . eglot-code-actions)
              ("M-s l" . flymake-show-project-diagnostics)
              ("M-s h" . eldoc))
  :config
  ;; C/C++
  ;; ------------------------------------------------
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "-j=12"
                    "--background-index"
                    "--clang-tidy"
                    "--compile-commands-dir=build"
                    "--query-driver=/usr/bin/clang++")))

  ;; Python (by default)
  ;; ------------------------------------------------
  ;; use "pyright" in virtualenv via /pyvenv/ mode
  ;; see "init-python.el"

  ;; MATLAB
  ;; ------------------------------------------------
  ;; Warning: eglot setup and matlab-ls seems buggy!
  ;; (add-to-list 'eglot-server-programs
  ;;              '(octave-mode . ("matlab-ls" "--stdio")))  ;; from ~/bin/
  ;; (setq-default eglot-workspace-configuration
  ;;               '(:MATLAB
  ;;                 (:indexWorkspace t
  ;;                                  :installPath "/Applications/MATLAB_R2022b.app"
  ;;                                  :matlabConnectionTiming "onStart")))

  ) ;End of eglot


(provide 'init-eglot)
;; ================================================
;; init-eglot.el ends here
