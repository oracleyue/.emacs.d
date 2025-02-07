;; ================================================================
;; Settings for /Org-mode/
;; ================================================================
;; Last modified on 22 Oct 2020

;; /Basics/
(global-font-lock-mode t)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link) ;; "C-c C-l" to insert
(global-set-key (kbd "C-c b") 'org-switchb)

(define-key org-mode-map (kbd "C-'") nil)

;; startup styles
(setq org-startup-folded     t
      org-startup-indented   t
      org-hide-leading-stars t)

;; set apps to open files in orgmode
(setq org-file-apps (quote ((auto-mode       . emacs)
                            ("\\.x?html?\\'" . default)
                            ("\\.pdf\\'"     . default))))

;; diminish minor ("Ind" keyword in powerbar)
(eval-after-load "org-indent"
  '(diminish 'org-indent-mode))

;; view styles (line wraping, fill-column)
(defun y/set-view-style-orgmode ()
  (setq truncate-lines t)
  (turn-off-auto-fill)
  (if *use-sans-orgmode*
      (progn (require 'org-variable-pitch)
             (org-variable-pitch-minor-mode t)
             (setq line-spacing '0.25)
             (setq-local fill-column 90))
    (setq-local fill-column 72)))
(add-hook 'org-mode-hook #'y/set-view-style-orgmode)

;; show inline images ("C-c C-x C-v" to display)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)  ;; try using width specified by #+attr_*

;; highlight latex fragments ("C-c C-x C-l" to preview)
(setq org-highlight-latex-and-related '(native latex script entities))

;; use cdlatex for fast math typing
;; note: firstly loaded in "init-auctex.el"
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; more pairings via /smartparens/
(with-eval-after-load 'smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]"
                   :trigger "\\["
                   :unless '(sp-point-after-word-p))
    ))

;; /GTD Function Extensions/
;; refer to http://doc.norang.ca/org-mode.html
(setq gtd-home       (expand-file-name "~/Public/Dropbox/GTD/"))
(setq todo-file      (expand-file-name "ToDoList.org" gtd-home))
(setq archive-file   (expand-file-name "ArchivedDiary.org" gtd-home))
(setq temp-todo-file (expand-file-name "inbox.org" gtd-home)) ;; used by iOS app

(setq org-archive-location (concat archive-file "::")) ;; "C-c C-x C-a"

(setq org-agenda-files (list todo-file temp-todo-file))
(setq org-capture-bookmark nil)  ;; disable auto-add bookmark

;; Capture templates
(setq org-capture-templates
      '(("t" "TODO (ToDoList)" entry (file+headline todo-file "Collecting")
         "* TODO %? \nDEADLINE: %^t\nAdded on %U" :empty-lines 1)
        ("s" "Scheduled (ToDoList)" entry (file+headline todo-file "Collecting")
         "* NEXT %? %^G \nSCHEDULED: %^t\nAdded on %U" :empty-lines 1)
        ("n" "Quick notes (ToDoList)" entry (file+headline todo-file "Notes")
         "* %?\nAdded on %U\n" :empty-lines 1)
        ;; research notes
        ;; ("i" "Ideas (Research)" entry (file idea-file)
        ;;  "* %?\nAdded on %U\n" :empty-lines 1)
        ;; ("m" "Seminar notes (Research)" entry (file seminar-file)
        ;;  "* %?\nAdded on %U\n" :empty-lines 1)
        ))

;; monthly gtd plan
(defun zyue/plan ()
  "Create a research diary for this month."
  (interactive)
  (progn (find-file todo-file)
         (goto-char (point-max))
         (insert "*" ?\s (format-time-string "%Y-%m %b") ?\n
                 "** Projects\n"
                 "** Research\n"
                 "** Review\n"
                 "** School\n"
                 "** Misc.\n"
                 "** Notes\n")))
;; archive monthly: use org-archive-subtree: "C-c C-x C-s"

;; Todo keywords
(defface org-doing
  '((t :foreground "white" :background "#75B5AA" :underline t))
  "Face for my own tag DOING."
  :group 'zyue)
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "DOING(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces '(("TODO"      . error)
                               ("WAITING"   . warning)
                               ("DONE"      . success)
                               ("NEXT"      . warning)
                               ("HOLD"      . default)
                               ("CANCELLED" . success)
                               ("DOING"     . org-doing))
      org-priority-faces '((?A . error)
                           (?B . warning)
                           (?C . success)))

;; Todo state triggers
(setq org-use-fast-todo-selection t)  ;; allow =C-c C-t= to enter KEY
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; /Export Settings/
;; html
(use-package htmlize)
;; add "#+HTML_HEAD" and "#+HTML_HEAD_EXTRA" in .org files
;; add "#+HTML_HEAD: " (leave empty) to disable global heads
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head
;;       (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
;;               (getenv "HOME")
;;               "/.emacs.d/templates/css/bootstrap.min.css\" />")
;;       org-html-head-extra
;;       (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
;;               (getenv "HOME")
;;               "/.emacs.d/templates/css/style.css\" />"))

;; use mathjax
;; #+OPTIONS: tex:t

;; markdown (built-in, or alternative /ox-gfm/)
;; (eval-after-load "org" '(require 'ox-md nil t))

;; /Code Blocks and Babel/
;; use syntax highlighting in org code blocks
(setq org-src-fontify-natively t)

;; use tab in .org to indent src blocks
(setq org-src-tab-acts-natively t)

;; setup babel for programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (C . t)
   (python . t)
   (R . t)
   (matlab . t)
   ;; (ledger . t)
   (latex . t)))

;; stop asking evaluation codes when export
(setq org-export-babel-evaluate nil)

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
             (assq-delete-all :exports org-babel-default-header-args)))

;; structure templates
(require 'org-tempo)

;; ------------------------------------------------------------
;; Minor Modes
;; ------------------------------------------------------------

;; /org-superstart/ for better UI
(use-package org-superstar
  :if (char-displayable-p ?⦿)
  :config
  (setq org-superstar-headline-bullets-list
        '("☰" "☷" "⦿" "✿" "✸" "●" "◆"))
  ;; avoid choosing unicode symbols intrinsically small
  ;; "☰" "☷" "☲" "☵" "⦿" "✿" "✸" "●" "⟐" "◆" "►"
  :hook (org-mode . org-superstar-mode))

;; /valign/: visual alignment for tables when using variable-pitch fonts
;; or Chinese
(use-package valign
  :demand
  :hook (org-mode . valign-mode))

;; /ox-gfm/: github flavored markdown (gfm) exporter
;; note: it preserves soft line breaks.
(use-package ox-gfm
  :demand
  :config
  (eval-after-load "org" '(require 'ox-gfm nil t)))

;; ------------------------------------------------------------
;; Presentation in Org Mode
;; ------------------------------------------------------------

;; /ox-reveal/: presentation via orgmode
(use-package ox-reveal
  :config
  (setq org-reveal-theme   "moon"
        org-reveal-hlevel   2
        org-reveal-progress t)
  ;; use css from github or locally
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  ;; (setq org-reveal-root "file:///home/zyue/reveal.js")
  ;; math support
  (setq org-reveal-mathjax t))

;; ------------------------------------------------------------
;; Notebook Workflow in Org mode
;; ------------------------------------------------------------

;; /org-download/ for image insertion
(use-package org-download
  :demand
  :bind (:map org-mode-map
              ;; drag-n-drop from os
              ;; drag-n-drop from browser (!bug in Chrome)
              ("C-c d s" . org-download-screenshot)  ;; screenshot
              ("C-c d y" . org-download-clipboard)   ;; paste from clipboard
              ("C-c d d" . org-download-yank)        ;; download from url
              ("C-c d D" . org-download-delete))
  :config
  (org-download-enable)
  (setq-default org-download-image-dir "./img/")
  ;; show inline image ("C-c C-x C-v" to toggle)
  (setq org-download-display-inline-images t
        org-download-image-attr-list
        '("#+ATTR_HTML: :width 480px :align center"))
  ;; screenshot: require config "security & privacy" in macOS
  (when *is-mac*
    (setq org-download-screenshot-method "screencapture -i %s"))
  :hook
  ;; disabled "yank-media" to ensure drag-drop behavior
  (org-mode . (lambda () (kill-local-variable 'dnd-protocol-alist)))
  ) ;End: org-download

;; /Citar/: front-end to browser and act on bibliographic data
(use-package citar
  :demand
  :custom
  (citar-bibliography '("~/Public/Dropbox/Academia/library.bib"))
  (org-cite-insert-processor   'citar)
  (org-cite-follow-processor   'citar)
  (org-cite-activate-processor 'citar)
  :hook
  (org-mode . citar-capf-setup)
  :bind
  (("C-c n o" . citar-open)   ;; trigger org-roam to write notes for papers
   :map org-mode-map
   ("C-c ]"   . org-cite-insert))
  :config
  (setq citar-notes-paths '("~/Public/Dropbox/RoamNotes/ref")
        citar-file-note-extensions '("org"))

  ;; embark supports
  (use-package citar-embark
    :after (citar embark)
    :hook  (org-mode . citar-embark-mode))
  ;; if you perfer the embark menu open with "org-open-at-point"
  ;; (setq citar-at-point-function 'embark-act)

  ;; icon supports
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-blue-alt)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon "nf-fa-file_pdf" :face 'nerd-icons-blue)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon "nf-md-notebook" :face 'nerd-icons-green)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons))
  ) ;End: citar

;; /org-roam/: bullet notes and organization
(use-package org-roam
  :demand
  :custom
  (org-roam-directory (file-truename "~/Public/Dropbox/RoamNotes"))
  (org-roam-dailies-directory "daily/")  ;; default diary directory
  ;; (org-roam-db-gc-threshold most-positive-fixnum)  ;; upgrade performance
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)  ;; show back-link window
         ("C-c n u" . org-roam-ui-mode)        ;; visualize in browser
         ;; tags
         ("C-c n a" . org-roam-tag-add)
         ("C-c n A" . org-roam-tag-remove)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)  ;; diary menu
  :config
  ;; basic
  (require 'org-roam-dailies)  ;; start diary function
  (org-roam-db-autosync-mode)  ;; sync with sqlite3 database

  ;; extend type for nodes
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; informative interface for vertical completion
  (setq org-roam-node-display-template
        (concat "${type:6} ${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

  ;; roam capture templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d>-${slug}.org"
                              "#+title: ${title}\n") :unnarrowed t)
          ("w" "work" plain "%?"
           :target (file+head "work/%<%Y%m%d>-${slug}.org"
                              "#+title: ${title}\n") :unnarrowed t)
          ("c" "cs" plain "%?"
           :target (file+head "cs/%<%Y%m%d>-${slug}.org"
                              "#+title: ${title}\n") :unnarrowed t)
          ("s" "seminar notes" plain "%?"
           :target (file+head "ref/%<%Y%m%d>-${slug}.org"
                              "#+title: ${title}\n") :unnarrowed t)
          ))

  ;; integration of /citar/ and /org-roam/
  (use-package citar-org-roam
    :demand
    :after (citar org-roam)
    :config
    (citar-org-roam-mode)
    ;; define capture template
    (setq citar-org-roam-note-title-template
          "${title}\nauthors: ${author}\nattachment: [[${file}][pdf]]")
    (add-to-list 'org-roam-capture-templates
                 `("n" "paper notes" plain "%?"
                   :target(file+head "ref/${citar-citekey}.org"
                                     "#+title: ${note-title}\n#+filetags: :paper:\n#+created: %U\n#+last_modified: %U\n\n")
                   :unnarrowed t))
    (setq citar-org-roam-capture-template-key "n"))
  ) ;End: org-roam

;; browser UI for /org-roam/
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme      t)  ;; sync emacs theme
  (org-roam-ui-follow          t)      ;; following note nodes
  (org-roam-ui-update-on-save  t))

;; /org-roam/ usage:
;; - create or search notes: "C-c n f"
;; - create paper notes: "M-x citar-open"
;; - insert ref: "C-c n i", or [[]] then press <tab> to complete
;; - open link: C-c C-o, or use "embark"
;; - show backlinks: "C-c n l"
;; advanced:
;; - manual update of database: M-x org-roam-db-sync
;; - convert a headline into a node: "org-id-get-create", "org-roam-refile"


(provide 'init-orgmode)
;; ================================================
;; init-orgmode.el ends here
