;;; clips-mode.el --- Clips editing mode.

;;;************************************************************************
;;; Basado en jess-mode.el de:

;; Copyright (C) 1999 by David E. Young.

;; Author: David E. Young <david.young@fnc.fujitsu.com>
;; Keywords: languages, clips

;; This is version 0.5 of 9 August 1999.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA
;;;************************************************************************

;;; Code:

(require 'lisp-mode)

(defvar clips-mode-map nil)

(defvar clips-mode-hook nil
  "*Hooks for customising Clips mode.")

(defvar clips-mode-syntax-table nil
  "The value of which is the syntax table for Clips mode.")

(when (not clips-mode-map)
  (let ((map (make-sparse-keymap "Clips")))
    (setq clips-mode-map
      (nconc (make-sparse-keymap) lisp-mode-shared-map))
;;; 15-6-2005,JLRR: cambiado shared-lisp-mode-map por lisp-mode-shared-map
    (define-key clips-mode-map [menu-bar] (make-sparse-keymap))
    (define-key clips-mode-map [menu-bar clips]
      (cons "Clips" map))
    (define-key map [inf-clips-load-file] 
      '("Load File" . inf-clips-load-file))
    (define-key map [run-clips] '("Run Inferior Clips" . run-clips))
    (define-key map [comment-region] '("Comment Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))))

(if (not clips-mode-syntax-table)
    (let ((i 0))
      (setq clips-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
             (modify-syntax-entry i "_   " clips-mode-syntax-table)
             (setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
             (modify-syntax-entry i "_   " clips-mode-syntax-table)
             (setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
             (modify-syntax-entry i "_   " clips-mode-syntax-table)
             (setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
             (modify-syntax-entry i "_   " clips-mode-syntax-table)
             (setq i (1+ i)))
      (modify-syntax-entry ?  "    " clips-mode-syntax-table)
      (modify-syntax-entry ?\t "    " clips-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " clips-mode-syntax-table)
      (modify-syntax-entry ?\f ">   " clips-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " clips-mode-syntax-table)
      (modify-syntax-entry ?` "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?' "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?, "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?. "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?# "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " clips-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " clips-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " clips-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " clips-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " clips-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " clips-mode-syntax-table)
      (modify-syntax-entry ?*   "w   " clips-mode-syntax-table)
      ;; The next syntax entry doesn't work with these forms:
      ;;  `,.foo
      ;;  #.foo
      ;; but it works better with variables with .'s in them
      (modify-syntax-entry ?. "w   " clips-mode-syntax-table)
      (modify-syntax-entry ?\| "_   " clips-mode-syntax-table)
      (modify-syntax-entry ?\[ "_   " clips-mode-syntax-table)
      (modify-syntax-entry ?\] "_   " clips-mode-syntax-table)))

(defconst clips-font-lock-keywords-1
  (eval-when-compile
    (let ((clips-constructs
           (regexp-opt
            '("deffunction" "deftemplate" "defrule" "deffacts" "defgeneric"
              "defmodule" "defadvice" "defglobal" "defmethod"
              "definstance" "defclass")))
         (clips-identifier
          (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                (digit "0-9"))
            (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))))
     (list
      (cons (concat "\\<" clips-constructs "\\>\\s *" clips-identifier)
            `(,(+ 1 (regexp-opt-depth clips-constructs)) font-lock-function-name-face))
      (cons (concat "\\<\\(" clips-constructs "\\)\\>") 'font-lock-keyword-face))))
  "Subdued expressions to highlight in Clips modes.")

(defconst clips-font-lock-keywords-2
  (append clips-font-lock-keywords-1
          (eval-when-compile
            (let ((clips-builtins
                   (regexp-opt
                    '("slot" "multislot" "type" "default" "default-dynamic"
                      "extends" "crlf""range" "nil" "if" "then" "else" "while"
                      "progn" "progn$" "not" "or" "switch" "case" "and" "reset"
                      "assert" "test" "declare" "salience" "return" "bind"
                      "retract" "explicit" "unique" "node-index-hash" "halt"
                      "=>")))
                  (clips-connective-constraints
                   (regexp-opt '("|" "&"))))
              (list
               (cons (concat "\\<\\(" clips-builtins "\\)\\>") 'font-lock-builtin-face)
               (cons (concat "\\<\\(" clips-connective-constraints "\\)\\>")
                     'font-lock-builtin-face)))))
  "Gaudy expressions to highlight in Clips modes.")

(defvar clips-font-lock-keywords clips-font-lock-keywords-2
  "Default expressions to highlight in Clips modes.")

(defun clips-initialize-mode ()
  (set-syntax-table clips-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lisp-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(clips-font-lock-keywords))
  (use-local-map clips-mode-map)
  (set-syntax-table clips-mode-syntax-table))

(put 'else 'clips-indent-function 0)

(defun clips-indent-function (ipoint state)
  (message "clips-indent-function")
  (lisp-indent-function ipoint state))

(defun clips-mode ()
  "Major mode for editing Clips code.
Editing commands are similar to those of other Lisp-like modes.

In addition, if an inferior Clips process is running, some additional
commands will be defined for evaluating expressions and controlling
the interpreter. The status of the process will also be displayed in
the modeline of all Clips buffers.

Commands:
\\{clips-mode-map}
Entry to this mode calls the value of `clips-mode-hook' if that value
is non-nil."
  (interactive)
  (kill-all-local-variables)
  (clips-initialize-mode)
  (setq major-mode 'clips-mode)
  (setq mode-name "Clips")
  (run-hooks 'clips-mode-hook))

(provide 'clips-mode)

;;; clips-mode.el ends here
