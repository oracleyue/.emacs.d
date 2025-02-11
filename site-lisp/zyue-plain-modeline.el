;; This provides a plain modeline.
;; Last modified on 02 Mar 2021

;; faces
(make-face 'y/mode-line-rownum-face)
(make-face 'y/mode-line-buffer-name-face)
(make-face 'y/mode-line-plain-face)

(when (eq 'solarized (car custom-enabled-themes))
  (set-face-attribute 'y/mode-line-buffer-name-face nil
                      :bold t))
(unless (eq 'solarized (car custom-enabled-themes))
  (set-face-attribute 'y/mode-line-buffer-name-face nil
                      :foreground
                      (face-foreground 'font-lock-function-name-face))
  (set-face-attribute 'y/mode-line-rownum-face nil
                      :foreground
                      (face-foreground 'font-lock-constant-face)))

;; configure box style
(when (eq 'atom-one-dark (car custom-enabled-themes))
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :style released-button)))

;; config for terminal
(when (and (not (daemonp)) (not window-system))
  (set-face-attribute 'mode-line nil
                      :background "color-238")
  (set-face-attribute 'mode-line-inactive nil
                      :background "color-236"))

;; format default mode line
(setq-default mode-line-format
              (list
               ;; default part
               "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'y/mode-line-buffer-name-face
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "("
               (propertize "%02l" 'face 'y/mode-line-rownum-face)
               ","
               (propertize "%02c")
               ") "

               ;; relative position, size of file
               "["
               (propertize "%p") ;; % above top
               "/"
               (propertize "%I") ;; size
               "] "

               ;; the current major mode for the buffer
               "["
               '(:eval (propertize (if (listp mode-name)
                                       (mapconcat 'identity (cdr mode-name) "/")
                                     mode-name)
                                   'help-echo buffer-file-coding-system))
               "] "
               ;; "      "
               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face
                                   (if overwrite-mode 'font-lock-warning-face nil)
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod" 'face 'error
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO" 'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "

               ;; load nyan nyan
               '(:eval (when nyan-mode (list (nyan-create))))

               ;; add the time, with the date and the emacs uptime in the tooltip
               "  ---"
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               "---"
               ))


(provide 'zyue-plain-modeline)
;; ================================================================
;; zyue-plain-modeline.el ends here
