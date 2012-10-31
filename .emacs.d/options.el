;; startup options
(setq inhibit-splash-screen t
      initial-scratch-message nil
      truncate-partial-width-windows nil)

(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; ido
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; line numbers
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d  ")

;; tabs, spaces, and whitespace, oh my!
(setq tab-width 2
      indent-tabs-mode nil)

;; ()s
(show-paren-mode t)

;; enable copying from system clipboard
(setq x-select-enable-clipboard t)

;; forget the backup files
(setq make-backup-files nil)

;; treat regions as other programs do
(delete-selection-mode t)

;; make flyspell mode behave
(setq flyspell-issue-welcome-flag nil)

;; font scaling
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; inferior lisp
(setq inferior-lisp-program "lein repl")

;; take care of utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(custom-set-variables '(slime-net-coding-system (quote utf-8-unix)))

;; account for zsh config files
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; make buffer list sane
(require 'kpm-list)

;; making slides with code on them no longer requires textmate
(require 'htmlize)

;; turn off needless/annoying things
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)

;; org mode dealies
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "INPR" "WAITING" "DONE")))
(setq org-todo-keyword-faces
      '(("INPR" . (:background "green" :foreground "white" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))))
(setq org-agenda-files (list "~/notes/relevance.org"
                             "~/notes/personal.org"
                             "~/notes/clojure.org"
                             "~/notes/songs.org"))

;; org babel config
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  "Evaluate a block of Clojure code with Babel."
  (lisp-eval-string body)
  "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

;; os x stuffs
(when (eq system-type 'darwin)
  (progn
    (setq grep-find-use-xargs 'exec
	  ispell-program-name "aspell"
	  magit-git-executable "/usr/local/bin/git")))

(provide 'options)

;; Markdown

(eval-after-load "markdown-mode"                                                                                                                                                      '(progn
     (require 'longlines)
     (progn
       (add-hook 'markdown-mode-hook 'longlines-mode))))

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.page$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; JavaScript

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; slime-js
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

