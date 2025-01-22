;; ================================================================
;; Tree-sitter for better code parsing
;; ================================================================
;; Last modified on 11 Jan 2025

;; Auto configure tree-sitter
(use-package treesit-auto
  :demand
  :init
  (setq treesit-auto-install    'prompt
        treesit-font-lock-level 3)    ; 4 for everything
  :config
  (global-treesit-auto-mode)
  ;; fix for Yasnippet: workaround for https://github.com/renzmann/treesit-auto/issues/76
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist)))

;; Extended functions based on tree-sitter
;; Note: having been merged into /expand-region/
(defun er/mark-ts-node ()
  "Expand region using tree-sitter parsing."
  (interactive)
  (when (and (boundp 'treesit-primary-parser) treesit-primary-parser)
    (let* ((node (if (use-region-p)
                     (treesit-node-on (region-beginning) (region-end))
                   (treesit-node-at (point))))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; when the node fits the region exactly, try its parent node instead
      (when (and (= (region-beginning) node-start)
                 (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (goto-char node-start)
      (set-mark node-end))))


(provide 'init-treesit)
;; ================================================
;; init-treesit.el ends here
