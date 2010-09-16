;; ===========================================
;; ========== Major and Minor modes ==========
;; ===========================================

(require 'color-theme)
(load-theme 'zenburn)
(color-theme-zenburn)

(autoload 'zencoding-mode "zencoding-mode.el" "Major mode for ZenCoding" t)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; ========== Turn on Markdown mode ==========
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.page$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; ========== Turn on Steve Yegge's js2 mode ==========
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ========== Turn on the ix mode ==========
(autoload 'ix-mode "ix.el" "Major mode for editing Ix files" t)
(add-to-list 'auto-mode-alist '("\\.ix$" . ix-mode))

;; ========== Turn on CLIPS mode ==========
(autoload 'clips-mode "clips-mode.el" "Major mode for editing Clips files" t)
(add-to-list 'auto-mode-alist '("\\.clips$" . clips-mode))

;; ========== Turn on Clojure mode ==========
(autoload 'clojure-mode "clojure-mode.el" "Major mode for editing Clojure files" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(eval-after-load "markdown-mode"                                                                                                                                                      '(progn
     (require 'longlines)
     (progn
       (add-hook 'markdown-mode-hook 'longlines-mode))))


;; ========== Turn on Scala mode ==========
(autoload 'scala-mode "scala-mode.el" "Major mode for editing Scala files" t)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; ========== Customize C mode ==========
(setq c-default-style
  '((c-mode . "stroustrup") (awk-mode . "awk") (other . "gnu")))


(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

;; ========== Confluence Mode ===========

(require 'confluence)

;; note, all customization must be in *one* custom-set-variables block
(custom-set-variables
 ;; confluence customization
 '(confluence-url "https://portal/rpc/xmlrpc")
 '(confluence-default-space-alist (list (cons confluence-url "~mfogus"))))

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))

    
     (add-hook 'ediff-cleanup-hook 
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))

;; keybindings (change to suit)

;; open confluence page
(global-set-key "\C-x\C-w" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-x\C-e" confluence-prefix-map)))

;; ========== Untabify certain files ==========
(defun mode-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

;; Java untabify
(add-hook 'java-mode-hook 
  '(lambda ()
     (setq c-basic-offset 4)
     (make-local-variable 'write-contents-hooks)
     (add-hook 'write-contents-hooks 'mode-untabify)))

;; C untabify
(add-hook 'c-mode-hook 
  '(lambda ()
     (make-local-variable 'write-contents-hooks)
     (add-hook 'write-contents-hooks 'mode-untabify)))

;; Ix untabify
(add-hook 'ix-mode-hook 
  '(lambda ()
     (make-local-variable 'write-contents-hooks)
     (add-hook 'write-contents-hooks 'mode-untabify)))
