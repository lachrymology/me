(require 'cl)

(defvar emacs-root (if 
                     (or (eq system-type 'cygwin)
                         (eq system-type 'gnu/linux)
                         (eq system-type 'linux))
                     "/home/fogusm/.emacs.d/"
                     (if (eq system-type 'darwin)
                       "/Users/fogusm/.emacs.d/"
                       "c:/home/fogusm/.emacs.d/"))
  "My home directory — the root of my personal emacs load-path.")

(labels ((add-path (p) 
           (add-to-list 'load-path (concat emacs-root p))))
  (add-path "me")          ;; all my personal elisp code
  (add-path "modes")
  (add-path "modes/scala-mode"))

(load-library "settings")  ;; my personal display settings
(load-library "keys")      ;; key bindings
(load-library "cfg")       ;; configuration for all of my modes
(load-library "code")      ;; my code

(require 'yasnippet) 
(yas/initialize)
(yas/load-directory (concat emacs-root "modes/snippets"))

(server-start)
(shell "*shell-main*")
(shell "*shell-irb*")
