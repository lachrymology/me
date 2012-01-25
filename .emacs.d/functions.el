
;; clojure repl fns
(defun clojure-repl ()
  (interactive)
  (inferior-lisp "java -jar /Users/fogus/Desktop/Dropebox/projects/clj/clojure/clojure-1.4.0-master-SNAPSHOT.jar"))

;; buffer clean up fns
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; vim style zap to character
(defun zap-until-char (arg char)
  "Kill up to ARGth occurrence of CHAR. Case is ignored 
  if `case-fold-search' is non-nil in the current buffer.
  Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap until char: ")
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (1- (point)))
               (backward-char)))

(global-set-key (kbd "M-z") 'zap-until-char) 

(defun fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
        ".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
         (concat "http://www.amazon.com/o/asin/"
                 (match-string 1)
                 (match-string 3)
                 "?tag=fogus-20"))))

(provide 'functions)
