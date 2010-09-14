;; Remove some GUI junk
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)
(setq standard-indent 4)

;;
;; Emacs normally uses both tabs and spaces to indent lines. If you
;; prefer, all indentation can be made from spaces only. To request this,
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.

;;
;; Use (setq ...) to set value locally to a buffer
;; Use (setq-default ...) to set value globally
;;
(setq-default indent-tabs-mode nil) 

;; Support Wheel Mouse Scrolling
(mouse-wheel-mode t)

;; Prevent Emacs from making backup files
(setq make-backup-files nil) 

;; Enable Line and Column Numbering
;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

; Turn on font-lock in supported modes
(if (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

; Display stuff
(setq font-lock-maximum-decoration t)
(require 'paren)
(show-paren-mode 1)
(autoload 'hide-ifdef-mode "hideif" nil t)

;; Aliases
(defalias 'dir 'dired)
