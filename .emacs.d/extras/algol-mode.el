;;; a68-mode.el --- Major mode for editing Algol 68 code

;; Copyright (C) 2011 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing Algol 68 code.
;;
;; TODO: support quote and dot stropping.

;;; Code:

(require 'syntax)
(require 'font-lock)

(defvar a68-indent-step 3
  "Indentation step for Algol 68")

(defvar a68-mode-hook nil)

(defvar a68-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\r"   'electric-a68-terminate-line)
    (define-key map "\t"   'electric-a68-tab)
    map)
  "Keymap for Algol 68 major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.a68\\'" . a68-mode))

(defconst a68-font-lock-keywords
  (list
   (cons (concat "\\<\\("
                 "DECS\\|PROGRAM\\|CONTEXT\\|USE\\|FINISH\\|KEEP"
                 "\\|ALIEN"
                 "\\|MODE\\|OP\\|PRIO\\|PROC"
                 "\\|PROC"
                 "\\|OF\\|AT\\|IS\\|ISNT\\|EMPTY\\|SKIP"
                 "\\|PR\\|PRAGMAT"
                 "\\|CASE\\|IN\\|OUSE\\|OUT\\|ESAC\\|"
                 "\\|FOR\\|FORALL\\|FROM\\|TO\\|BY\\|WHILE\\|DO\\|OD"
                 "\\|IF\\|THEN\\|ELIF\\|THEN\\|ELSE\\|FI"
                 "\\|PAR\\|BEGIN\\|END\\|GOTO\\|EXIT"
                 "\\|LWB\\|UPB\\|NOT\\|ABS\\|BIN\\|REPR\\|LENG\\|SHORTEN\\|ODD\\|SIGN\\|ROUND\\ENTIER"
                 "\\|AND\\|OR\\|DIV\\|OVER\\|MOD\\|ELEM\\|SHL\\|SHR\\|IS\\|ISNT"
                 "\\|OVERAB\\|DIVAB\\|MODAB"
                 "\\|REF"
                 "\\)\\>")
         'font-lock-keyword-face)
   (cons (concat "\\<\\("
                 "TRUE\\|\\FALSE"
                 "\\)\\>")
         'font-lock-constant-face)
   ;; Note that the following rule is only valid for bold stropping.
   (cons (concat "\\<[A-Z]+\\>") 'font-lock-type-face)
  
   (cons "\\('\\w*'\\)"
         font-lock-variable-name-face))
  "Highlighting expressions for Algol 68 mode")

(defun a68-within-string ()
  (nth 3 (syntax-ppss)))

(defun a68-within-comment ()
  (nth 4 (syntax-ppss)))

;; Indentation rules:
;;
;; - If we are at the beginning of the buffer, or looking at some
;;   indent-0 content, indent to column 0.
;;
;; - If we are currently at an END, ), FI or OD, then de-indent
;;   relative to the previous line.
;;
;; - If we first see and "end line" before our current line,
;;   then we should indent our current line to the same indentation as
;;   the end line.
;;
;; - If we first see a "start line" like IF, then we need to increase
;;   our indentation relative to that start line.
;;
;; - If into a balanced expression, we should indent to the column
;;   where the start of the innermost parenthetical group.
;;
;; - If none of the above apply, then do not indent at all.

(defun a68-indent-line ()
  "Indent current line as Algol 68 code"
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (nth 1 (syntax-ppss)) ; Check for rule 5
          (let ((offset (save-excursion (goto-char (+ (nth 1 (syntax-ppss)) 1))
                                        (current-column))))
            (indent-line-to offset))
        (if (or (bobp) ; Check for rule 1
                (looking-at "^[ \t]*\\<\\(KEEP\\|FINISH\\|DECS\\|USE\\|PROGRAM\\)\\>"))
            (indent-line-to 0)
          (let ((not-indented t)
                (prev-indent (current-indentation))
                (begin-indent-re "^[ \t]*\\<\\(BEGIN\\|KEEP\\|IF\\|DO\\|ELSE\\|ELIF\\|THEN\\)")
                (deindent-line-re "^[ \t]*\\<\\(END\\|FI\\|OD\\|ELSE\\|ELIF\\)\\>")
                (eqindent-line-re "^[ \t]*\\<\\(THEN\\)\\>")
                (end-line-re "^[ \t]*\\(END\\|FI\\|OD\\)")
                cur-indent)
            (if (looking-at eqindent-line-re)
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (current-indentation)))
              (if (looking-at deindent-line-re) ; Check for rule 2
                  (progn
                    (save-excursion
                      (forward-line -1)
                      (setq cur-indent (- (current-indentation) a68-indent-step)))
                    (if (< cur-indent 0)
                        (setq cur-indent 0)))
                (save-excursion
                  (while not-indented
                    (forward-line -1)
                    (if (looking-at end-line-re) ; Check for rule 3
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                      ;; Check for rule 4
                      (if (looking-at begin-indent-re)
                          (progn
                            (setq cur-indent (+ (current-indentation) a68-indent-step))
                            (setq not-indented nil))
                        (if (bobp) ; Check for rule 5
                            (setq not-indented nil))))))))
            (if cur-indent
                (indent-line-to cur-indent)
              ;; If we didn't see an indentation hint, then allow no
              ;; indentation.
              (indent-line-to 0)))))))
  (when (< (current-column) (current-indentation))
    (move-to-column (current-indentation))))

(defvar a68-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?{ "<" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?} ">" st)
    (modify-syntax-entry ?# ">" st)
    (modify-syntax-entry ?\\ "." st)
;;    (modify-syntax-entry ?C "< 13" st)
;;    (modify-syntax-entry ?O "> 24" st)
    ;; define parentheses to match
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

;;;
;;; Electric functions
;;;

(defconst a68-autoindent-lines-re
  "\\<\\(BEGIN\\|END\\|ELSE\\|ELIF\\|DO\\|OD\\|CASE\\|ESAC\\|IN\\|OUT\\)\\>")

(defun electric-a68-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at a68-autoindent-lines-re)
        (a68-indent-line)))
  (delete-horizontal-space) ; Removes triling whitespaces
  ;; Indent next line if we are not in a string
  (let ((in-string (a68-within-string)))
    (newline)
    (unless in-string
      (a68-indent-line))))

(defun electric-a68-tab ()
  "Function called when TAB is pressed in Algol68 mode."
  (interactive)
  (unless (save-excursion
            (beginning-of-line)
            (a68-within-string))
    (a68-indent-line)))

(defun algol-mode ()
  "Major mode for editing Algol 68 files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table a68-mode-syntax-table)
  (use-local-map a68-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(a68-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'a68-indent-line)
  (setq major-mode 'a68-mode)
  (setq mode-name "Algol68")
  (run-hooks 'a68-mode-hooks))

(provide 'algol-mode)

