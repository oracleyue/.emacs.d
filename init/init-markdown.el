;; ================================================================
;; /Markdown/: major mode for Markdown
;; ================================================================

;; Install packages in the system
;; - require "multimarkdown" or "markdown" in shell

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . (lambda () (turn-off-auto-fill)))
  :config
  ;; use variable-width fonts
  (defun zyue/markdown-use-variable-pitch ()
    "Use variable-width fonts in Markdown mode."
    (interactive)
    (variable-pitch-mode t)
    (setq-local fill-column 90)
    ;; restore fixed-pitch for codes
    (let ((font-name (face-attribute 'default :family)))
      (set-face-font 'markdown-pre-face  font-name)
      (set-face-font 'markdown-code-face font-name)
      (set-face-font 'markdown-inline-code-face font-name)))

  ;; fontify code blocks
  (setq markdown-fontify-code-blocks-natively t)

  ;; face refinement
  (set-face-background 'markdown-code-face (face-background 'org-block))
  (set-face-background 'markdown-pre-face (face-background 'org-block))

  ;; convert into org file
  (defun markdown-pandoc-convert (&optional newtype)
    "Converts the current buffer, assumed to be in Markdown format,
into a new format.  The new format must be one acceptable to
`Pandoc'.  The function opens the new file in a buffer if called
interactively.  If not called interactively then it returns the
name of the new file."
    (interactive "sOutput [default: org]: ")
    (when (string= newtype "") (setq newtype "org"))
    (let ((current-document (buffer-file-name))
          (temp-filename (concat "/tmp/output." newtype)))
      (with-temp-file temp-filename
        (call-process-shell-command (concat "pandoc -f markdown -t " newtype)
                                    nil t nil current-document))
      (cond ((called-interactively-p 'any)
             (with-current-buffer (find-file temp-filename)))
            (t temp-filename))))
  (define-key markdown-mode-command-map (kbd "d") 'markdown-pandoc-convert)

  ;; outline view of headings
  ;; use /imenu-list/ in "init-dired", default toggled by "C-x C-'"
  (use-package imenu-list
    :bind ((:map markdown-mode-map
                ("C-c =" . imenu-list-smart-toggle))
           (:map imenu-list-major-mode-map
                 ("C-c =" . imenu-list-smart-toggle))))

  ;; configure compile commands
  (if (string-equal system-type "darwin")
      (progn
        (setq markdown-command "/usr/local/bin/multimarkdown")
        (setq markdown-open-command "/Users/zyue/bin/Marked2"))
    (setq markdown-command "/usr/bin/multimarkdown"))

  ;; configure markdown export styles
  (add-to-list 'markdown-css-paths
               (concat (expand-file-name "~/.emacs.d/templates/css/") "github.md.css"))
  ;; using "style.md.css" requires "bootstrap.min.css"

  ;; use mathjax
  (setq markdown-xhtml-header-content
        (concat "<script type=\"text/javascript\" async src=\""
                "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_HTML"
                "\"></script>"))

  ) ;END of use-package(markdown-mode)


(provide 'init-markdown)
;; ================================================
;; init-markdown.el ends here
