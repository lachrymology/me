(add-to-list 'load-path "~/.emacs.d")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "JAVA_HOME" "/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home")
(setenv "CLOJURESCRIPT_HOME" "/Users/fogus/Desktop/Dropbox/projects/clj/clojurescript")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'cl)
(require 'el-get)
(require 'package)

(dolist (archive '(("marmalade" . "http://marmalade-repo.org/packages/")
		   ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives archive))
(package-initialize)

(setq el-get-sources
      '((:name paredit
	       :after (progn
			(let ((paredit-modes '(clojure
					       emacs-lisp
					       lisp
					       lisp-interaction
					       scheme)))
			  (dolist (mode paredit-modes)
			    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
				      (lambda () (paredit-mode +1)))))
			(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))
	(:name clojure-mode 
	       :type elpa
	       :after (progn
			(add-to-list 'auto-mode-alist '("\\.clj.*$" . clojure-mode))))
	(:name slime-repl :type elpa)
	;;	(:name slime
	;;       :type elpa
	;;       :after
	;;	(lambda ()
	;;  (setq slime-protocol-version 'ignore)
	;;  (setq font-lock-verbose nil)))
	;; (:name smex
	;;        :type elpa
	;;        :after (lambda ()
	;; 		(smex-initialize)
	;; 		(global-set-key (kbd "M-x") 'smex)
	;; 		(global-set-key (kbd "M-X") 'smex-major-mode-commands)))
	;; (:name deft
	;;        :type elpa
	;;        :after (lambda ()
	;; 		(setq deft-extension "org")
	;; 		(setq deft-directory "~/Desktop/Dropbox/notes")
	;; 		(setq deft-text-mode 'org-mode)
	;; 		(global-set-key [f3] 'deft)))
))

(setq my-packages
      (append
       '(ac-slime
	 auto-complete
	 coffee-mode
	 ;; deft
	 elein
	 el-get
	 js2-mode
	 markdown-mode
	 ;;org-mode
	 ruby-block
	 ruby-end
	 ruby-mode
	 scala-mode
	 swank-clojure
	 textile-mode
	 ;;smex
	 yaml-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(require 'options)
(require 'functions)
(put 'ido-exit-minibuffer 'disabled nil)


;; extras

;; ALGOL

(autoload 'algol-mode "extras/algol-mode.el" "Major mode for editing ALOGOL source" t)
(add-to-list 'auto-mode-alist '("\\.a68.*$" . algol-mode))
(add-to-list 'auto-mode-alist '("\\.algol.*$" . algol-mode))

;; NOTROFF

(autoload 'notroff-mode "extras/notroff-mode.el" "Major mode for editing NOTROFF docs." t)
(add-to-list 'auto-mode-alist '("\\.nr.*$" . notroff-mode))
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mediawiki-site-alist (quote (("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page") ("MATREX" "http://saturn/wiki/" "mfogus" "" "Main Page"))))
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix) (org-export-html-style-include-scripts))))
 '(slime-net-coding-system (quote utf-8-unix)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

(require 'org-exp-blocks)

(defun toggle-tags-invisible ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (not (remove-text-properties (point-min) (point-max) '(invisible t)))
     (let ((spew (make-progress-reporter "Hiding all tags..." (point-min) (point-max))))
       (while (re-search-forward "<indexterm .*?</indexterm>\\\|<.*?>" nil t)
         (add-text-properties (match-beginning 0) (match-end 0) '(invisible t) nil)
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))))

(add-hook 'nxml-mode-hook
          (lambda ()
            (visual-line-mode)))

