(require 'easymenu)
(require 'outline)
(eval-when-compile (require 'cl))

;;; Constants =================================================================

(defconst notroff-mode-version "0.0.1"
  "Notroff mode version number.")

(defconst notroff-output-buffer-name "*notroff-output*"
  "Name of temporary buffer for notroff command output.")

;;; Customizable variables ====================================================

(defvar notroff-mode-hook nil
  "Hook run when entering Notroff mode.")

(defgroup notroff nil
  "Major mode for editing text files in Notroff format."
  :prefix "notroff-"
  :group 'wp
  :link '(url-link "http://fogus.me"))

(defcustom notroff-command "notroff"
  "Command to run notroff."
  :group 'notroff
  :type 'string)

(defcustom notroff-command-needs-filename nil
  "Set to non-nil if `notroff-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command-line
option.  As a result, you will only be able to run Notroff from
buffers which are visiting a file."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-hr-string "* * * * *"
  "String to use for horizonal rules."
  :group 'notroff
  :type 'string)

(defcustom notroff-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-indent-function 'notroff-indent-line
  "Function to use to indent."
  :group 'notroff
  :type 'function)

(defcustom notroff-indent-on-enter t
  "Automatically indent new lines when enter key is pressed."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-follow-wiki-link-on-enter t
  "Follow wiki link at point (if any) when the enter key is pressed."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-wiki-link-alias-first t
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]]."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'notroff
  :type 'list)

(defcustom notroff-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'notroff
  :type 'boolean)

(defcustom notroff-css-path ""
  "URL of CSS file to link to in the output XHTML."
  :group 'notroff
  :type 'string)

(defcustom notroff-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'notroff
  :type 'string)

(defcustom notroff-xhtml-standalone-regexp
  "^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)"
  "Regexp indicating whether `notroff-command' output is standalone XHTML."
  :group 'notroff
  :type 'regexp)

(defcustom notroff-link-space-sub-char
  "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'notroff
  :type 'string)

(defcustom notroff-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'notroff
  :type '(choice (const :tag "At the end of the document" end)
		 (const :tag "Immediately after the paragraph" immediately)
		 (const :tag "Before next header" header)))

;;; Font lock =================================================================

(require 'font-lock)

(defvar notroff-italic-face 'notroff-italic-face
  "Face name to use for italic text.")

(defvar notroff-bold-face 'notroff-bold-face
  "Face name to use for bold text.")

(defvar notroff-header-face 'notroff-header-face
  "Face name to use as a base for headers.")

(defvar notroff-header-face-1 'notroff-header-face-1
  "Face name to use for level-1 headers.")

(defvar notroff-header-face-2 'notroff-header-face-2
  "Face name to use for level-2 headers.")

(defvar notroff-header-face-3 'notroff-header-face-3
  "Face name to use for level-3 headers.")

(defvar notroff-header-face-4 'notroff-header-face-4
  "Face name to use for level-4 headers.")

(defvar notroff-header-face-5 'notroff-header-face-5
  "Face name to use for level-5 headers.")

(defvar notroff-header-face-6 'notroff-header-face-6
  "Face name to use for level-6 headers.")

(defvar notroff-inline-code-face 'notroff-inline-code-face
  "Face name to use for inline code.")

(defvar notroff-list-face 'notroff-list-face
  "Face name to use for list markers.")

(defvar notroff-blockquote-face 'notroff-blockquote-face
  "Face name to use for blockquote.")

(defvar notroff-pre-face 'notroff-pre-face
  "Face name to use for preformatted text.")

(defvar notroff-link-face 'notroff-link-face
  "Face name to use for links.")

(defvar notroff-missing-link-face 'notroff-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar notroff-reference-face 'notroff-reference-face
  "Face name to use for reference.")

(defvar notroff-footnote-face 'notroff-footnote-face
  "Face name to use for footnote identifiers.")

(defvar notroff-url-face 'notroff-url-face
  "Face name to use for URLs.")

(defvar notroff-link-title-face 'notroff-link-title-face
  "Face name to use for reference link titles.")

(defvar notroff-comment-face 'notroff-comment-face
  "Face name to use for HTML comments.")

(defvar notroff-math-face 'notroff-math-face
  "Face name to use for LaTeX expressions.")

(defgroup notroff-faces nil
  "Faces used in Notroff Mode"
  :group 'notroff
  :group 'faces)

(defface notroff-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for italic text."
  :group 'notroff-faces)

(defface notroff-bold-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for bold text."
  :group 'notroff-faces)

(defface notroff-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'notroff-faces)

(defface notroff-header-face-1
  '((t (:inherit notroff-header-face)))
  "Face for level-1 headers."
  :group 'notroff-faces)

(defface notroff-header-face-2
  '((t (:inherit notroff-header-face)))
  "Face for level-2 headers."
  :group 'notroff-faces)

(defface notroff-header-face-3
  '((t (:inherit notroff-header-face)))
  "Face for level-3 headers."
  :group 'notroff-faces)

(defface notroff-header-face-4
  '((t (:inherit notroff-header-face)))
  "Face for level-4 headers."
  :group 'notroff-faces)

(defface notroff-header-face-5
  '((t (:inherit notroff-header-face)))
  "Face for level-5 headers."
  :group 'notroff-faces)

(defface notroff-header-face-6
  '((t (:inherit notroff-header-face)))
  "Face for level-6 headers."
  :group 'notroff-faces)

(defface notroff-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'notroff-faces)

(defface notroff-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'notroff-faces)

(defface notroff-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'notroff-faces)

(defface notroff-pre-face
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'notroff-faces)

(defface notroff-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'notroff-faces)

(defface notroff-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'notroff-faces)

(defface notroff-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'notroff-faces)

(defface notroff-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'notroff-faces)

(defface notroff-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'notroff-faces)

(defface notroff-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'notroff-faces)

(defface notroff-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'notroff-faces)

(defface notroff-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'notroff-faces)

(defconst notroff-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst notroff-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst notroff-regex-reference-definition
  "^ \\{0,3\\}\\(\\[[^^]+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst notroff-regex-footnote
;;  "\\(\\{[^}]*\\}"
  "\{\{[^}]+\}\}"
  "Regular expression for a footnote marker {{this is a fn}}.")

(defconst notroff-regex-header
  "#+\\|\\S-.*\n\\(?:\\(===+\\)\\|\\(---+\\)\\)$"
  "Regexp identifying Notroff headers.")

(defconst notroff-regex-header-1-atx
  "^\\.section \\(.*?\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst notroff-regex-header-2-atx
  "^\\.\\.section \\(.*?\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst notroff-regex-header-3-atx
  "^\\.\\.\\.section \\(.*?\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst notroff-regex-header-4-atx
  "^\\.\\.\\.\\.section \\(.*?\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst notroff-regex-header-5-atx
  "^\\.chapter \\(.*?\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst notroff-regex-header-6-atx
  "^\\.author \\(.*?\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst notroff-regex-code-start
  "^\\.code\\(.*?\\)"
  "Regular expression for code bloc<k start tags")

(defconst notroff-regex-body-start
  "^\\.body\\(.*?\\)"
  "Regular expression for body start tags")

(defconst notroff-regex-include
  "^\\.inc \\(.*?\\)"
  "Regular expression for include tags")

(defconst notroff-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst notroff-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst notroff-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Notroff horizontal rules.")

(defconst notroff-regex-code
  "\\(^\\|[^\\]\\)\\(\\([@]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst notroff-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst notroff-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers.")

(defconst notroff-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([!]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst notroff-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([~]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching italic text.")

(defconst notroff-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst notroff-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst notroff-regex-wiki-link
  "\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]"
  "Regular expression for matching wiki links.
This matches typical bracketed [[WikiLinks]] as well as 'aliased'
wiki links of the form [[PageName|link text]].  In this regular
expression, #1 matches the page name and #3 matches the link
text.")

(defconst notroff-regex-uri
  (concat
   "\\(" (mapconcat 'identity notroff-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst notroff-regex-angle-uri
  (concat
   "\\(<\\)\\("
   (mapconcat 'identity notroff-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst notroff-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst notroff-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst notroff-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defconst notroff-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

(defvar notroff-mode-font-lock-keywords-basic
  (list
   '(notroff-match-pre-blocks 0 notroff-pre-face t t)
   '(notroff-match-fenced-code-blocks 0 notroff-pre-face t t)
   (cons notroff-regex-blockquote 'notroff-blockquote-face)
   (cons notroff-regex-header-1-setext 'notroff-header-face-1)
   (cons notroff-regex-header-2-setext 'notroff-header-face-2)
   (cons notroff-regex-header-1-atx 'notroff-header-face-1)
   (cons notroff-regex-header-2-atx 'notroff-header-face-2)
   (cons notroff-regex-header-3-atx 'notroff-header-face-3)
   (cons notroff-regex-header-4-atx 'notroff-header-face-4)
   (cons notroff-regex-header-5-atx 'notroff-header-face-5)
   (cons notroff-regex-header-6-atx 'notroff-header-face-6)
   (cons notroff-regex-code-start 'notroff-header-face-6)
   (cons notroff-regex-body-start 'notroff-header-face-6)
   (cons notroff-regex-include 'notroff-header-face-6)
   (cons notroff-regex-hr 'notroff-header-face)
   '(notroff-match-comments 0 notroff-comment-face t t)
   (cons notroff-regex-code '(2 notroff-inline-code-face))
   (cons notroff-regex-angle-uri 'notroff-link-face)
   (cons notroff-regex-uri 'notroff-link-face)
   (cons notroff-regex-email 'notroff-link-face)
   (cons notroff-regex-list 'notroff-list-face)
   (cons notroff-regex-link-inline
         '((1 notroff-link-face t)
           (2 notroff-url-face t)))
   (cons notroff-regex-link-reference
         '((1 notroff-link-face t)
           (2 notroff-reference-face t)))
   (cons notroff-regex-reference-definition
         '((1 notroff-reference-face t)
           (2 notroff-url-face t)
           (3 notroff-link-title-face t)))
   (cons notroff-regex-footnote 'notroff-footnote-face)
   (cons notroff-regex-bold '(2 notroff-bold-face))
   (cons notroff-regex-italic '(2 notroff-italic-face))
   )
  "Syntax highlighting for Notroff files.")

(defconst notroff-mode-font-lock-keywords-latex
  (list
   ;; Math mode $..$ or $$..$$
   (cons notroff-regex-latex-expression '(2 notroff-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons notroff-regex-latex-display 'notroff-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'notroff-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'notroff-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar notroff-mode-font-lock-keywords
  (append
   (if notroff-enable-math
       notroff-mode-font-lock-keywords-latex)
   notroff-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Notroff mode.")

;; Footnotes
(defvar notroff-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'notroff-footnote-counter)

(defconst notroff-footnote-chars
  "[[:alnum:]-]"
  "Regular expression maching any character that is allowed in a footnote identifier.")



;;; Compatibility =============================================================

;; Handle replace-regexp-in-string in XEmacs 21
(defun notroff-replace-regexp-in-string (regexp rep string)
  "Compatibility wrapper to provide `replace-regexp-in-string'."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))



;;; Notroff parsing functions ================================================

(defun notroff-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun notroff-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (notroff-cur-line-blank-p))))

(defun notroff-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (notroff-cur-line-blank-p))))

(defun notroff-prev-line-indent-p ()
  "Return t if the previous line is indented and nil otherwise."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun notroff-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^\\s +" (point-at-eol) t)
    (current-column)))

(defun notroff-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (notroff-cur-line-indent)))

(defun notroff-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (notroff-cur-line-indent)))

(defun notroff-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward notroff-regex-list-indent (point-at-eol) t)
      (current-column))))

(defun notroff-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line."
  (save-excursion
    (forward-line -1)
    (notroff-cur-non-list-indent)))

(defun notroff--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (notroff-prev-line-blank-p))
                  (notroff-cur-line-blank-p))
              (not (eobp)))
    (forward-line)))

(defun notroff--end-of-level (level)
  "Move the point to the end of region with indentation at least LEVEL."
  (let (indent)
    (while (and (not (< (setq indent (notroff-cur-line-indent)) level))
                (not (>= indent (+ level 4)))
                (not (eobp)))
      (notroff--next-block))
    (unless (eobp)
      ;; Move back before any trailing blank lines
      (while (and (notroff-prev-line-blank-p)
                  (not (bobp)))
        (forward-line -1))
      (forward-line -1)
      (end-of-line))))

; From html-helper-mode
(defun notroff-match-comments (last)
  "Match HTML comments from the point to LAST."
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun notroff-match-pre-blocks (last)
  "Match Notroff pre blocks from point to LAST.
A region matches as if it is indented at least four spaces
relative to the nearest previous block of lesser non-list-marker
indentation."

  (let (cur-begin cur-end cur-indent prev-indent prev-list stop match found)
    ;; Don't start in the middle of a block
    (unless (and (bolp)
                 (notroff-prev-line-blank-p)
                 (not (notroff-cur-line-blank-p)))
      (notroff--next-block))

    ;; Move to the first full block in the region with indent 4 or more
    (while (and (not (>= (setq cur-indent (notroff-cur-line-indent)) 4))
                (not (>= (point) last)))
      (notroff--next-block))
    (setq cur-begin (point))
    (notroff--end-of-level cur-indent)
    (setq cur-end (point))
    (setq match nil)
    (setq stop (> cur-begin cur-end))

    (while (and (<= cur-end last) (not stop) (not match))
      ;; Move to the nearest preceding block of lesser (non-marker) indentation
      (setq prev-indent (+ cur-indent 1))
      (goto-char cur-begin)
      (setq found nil)
      (while (and (>= prev-indent cur-indent)
                  (not (and prev-list
                            (eq prev-indent cur-indent)))
                  (not (bobp)))

        ;; Move point to the last line of the previous block.
        (forward-line -1)
        (while (and (notroff-cur-line-blank-p)
                    (not (bobp)))
          (forward-line -1))

        ;; Update the indentation level using either the
        ;; non-list-marker indentation, if the previous line is the
        ;; start of a list, or the actual indentation.
        (setq prev-list (notroff-cur-non-list-indent))
        (setq prev-indent (or prev-list
                              (notroff-cur-line-indent)))
        (setq found t))

      ;; If the loop didn't execute
      (unless found
        (setq prev-indent 0))

      ;; Compare with prev-indent minus its remainder mod 4
      (setq prev-indent (- prev-indent (mod prev-indent 4)))

      ;; Set match data and return t if we have a match
      (if (>= cur-indent (+ prev-indent 4))
          ;; Match
          (progn
            (setq match t)
            (set-match-data (list cur-begin cur-end))
            ;; Leave point at end of block
            (goto-char cur-end)
            (forward-line))

        ;; Move to the next block (if possible)
        (goto-char cur-end)
        (notroff--next-block)
        (setq cur-begin (point))
        (setq cur-indent (notroff-cur-line-indent))
        (notroff--end-of-level cur-indent)
        (setq cur-end (point))
        (setq stop (equal cur-begin cur-end))))
    match))

(defun notroff-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun notroff-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))



;;; Syntax Table ==============================================================

(defvar notroff-mode-syntax-table
  (let ((notroff-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" notroff-mode-syntax-table)
    notroff-mode-syntax-table)
  "Syntax table for `notroff-mode'.")



;;; Element Insertion =========================================================

(defun notroff-wrap-or-insert (s1 s2)
 "Insert the strings S1 and S2.
If Transient Mark mode is on and a region is active, wrap the strings S1
and S2 around the region."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (goto-char a)
       (insert s1)
       (goto-char (+ b (length s1)))
       (insert s2))
   (insert s1 s2)))

(defun notroff-insert-hr ()
  "Insert a horizonal rule using `notroff-hr-string'."
  (interactive)
  ;; Leading blank line
  (when (and (>= (point) (+ (point-min) 2))
             (not (looking-back "\n\n" 2)))
    (insert "\n"))
  ;; Insert custom HR string
  (insert (concat notroff-hr-string "\n"))
  ;; Following blank line
  (backward-char)
  (unless (looking-at "\n\n")
          (insert "\n")))

(defun notroff-insert-bold ()
  "Insert markup for a bold word or phrase.
If Transient Mark mode is on and a region is active, it is made bold."
  (interactive)
  (if notroff-bold-underscore
      (notroff-wrap-or-insert "__" "__")
    (notroff-wrap-or-insert "**" "**"))
  (backward-char 2))

(defun notroff-insert-italic ()
  "Insert markup for an italic word or phrase.
If Transient Mark mode is on and a region is active, it is made italic."
  (interactive)
  (if notroff-italic-underscore
      (notroff-wrap-or-insert "_" "_")
    (notroff-wrap-or-insert "*" "*"))
  (backward-char 1))

(defun notroff-insert-code ()
  "Insert markup for an inline code fragment.
If Transient Mark mode is on and a region is active, it is marked
as inline code."
  (interactive)
  (notroff-wrap-or-insert "`" "`")
  (backward-char 1))

(defun notroff-insert-link ()
  "Insert an inline link of the form []().
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (notroff-wrap-or-insert "[" "]")
  (insert "()")
  (backward-char 1))

(defun notroff-insert-reference-link-dwim ()
  "Insert a reference link of the form [text][label] at point.
If Transient Mark mode is on and a region is active, the region
is used as the link text. Otherwise, the link text will be read
from the minibuffer. The link URL, label, and title will be read
from the minibuffer. The link label definition is placed at the
end of the current paragraph."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'notroff-insert-reference-link-region)
    (call-interactively 'notroff-insert-reference-link)))

(defun notroff-insert-reference-link-region (url label title)
  "Insert a reference link at point using the region as the link text."
  (interactive "sLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (notroff-insert-reference-link text url label title)))

(defun notroff-insert-reference-link (text url label title)
  "Insert a reference link at point.
The link label definition is placed at the end of the current
paragraph."
  (interactive "sLink Text: \nsLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let (end)
    (insert (concat "[" text "][" label "]"))
    (setq end (point))
    (forward-paragraph)
    (insert "\n[")
    (if (> (length label) 0)
        (insert label)
      (insert text))
    (insert (concat "]: " url))
    (unless (> (length url) 0)
        (setq end (point)))
    (when (> (length title) 0)
      (insert (concat " \"" title "\"")))
    (insert "\n")
    (unless (looking-at "\n")
      (insert "\n"))
    (goto-char end)))

(defun notroff-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (notroff-wrap-or-insert "[[" "]]")
  (backward-char 2))

(defun notroff-insert-image ()
  "Insert an inline image tag of the form ![]().
If Transient Mark mode is on and a region is active, it is used
as the alt text of the image."
  (interactive)
  (notroff-wrap-or-insert "![" "]")
  (insert "()")
  (backward-char 1))

(defun notroff-insert-header-1 ()
  "Insert a first level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 1))

(defun notroff-insert-header-2 ()
  "Insert a second level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 2))

(defun notroff-insert-header-3 ()
  "Insert a third level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 3))

(defun notroff-insert-header-4 ()
  "Insert a fourth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 4))

(defun notroff-insert-header-5 ()
  "Insert a fifth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 5))

(defun notroff-insert-header-6 ()
  "Insert a sixth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (notroff-insert-header 6))

(defun notroff-insert-header (n)
  "Insert an atx-style (hash mark) header.
With no prefix argument, insert a level-1 header.  With prefix N,
insert a level-N header.  If Transient Mark mode is on and the
region is active, it is used as the header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr hdrl hdrr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))      ; Build a hash mark header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (notroff-wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

(defun notroff-insert-title ()
  "Insert a setext-style (underline) first level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "=" hdr)))  ; Build a === title underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n==========\n")
    (backward-char 12)))

(defun notroff-insert-section ()
  "Insert a setext-style (underline) second level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "-" hdr)))  ; Build a --- section underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n----------\n")
    (backward-char 12)))

(defun notroff-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (notroff-blockquote-region (region-beginning) (region-end))
    (insert "> ")))

(defun notroff-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (if mark-active
      (save-excursion
        ;; Ensure that there is a leading blank line
        (goto-char beg)
        (when (and (>= (point) (+ (point-min) 2))
                   (not (looking-back "\n\n" 2)))
          (insert "\n")
          (setq beg (1+ beg))
          (setq end (1+ end)))
        ;; Move back before any blank lines at the end
        (goto-char end)
        (while (and (looking-back "\n" 1)
                    (not (equal (point) (point-min))))
          (backward-char)
          (setq end (1- end)))
        ;; Ensure that there is a trailing blank line
        (goto-char end)
        (if (not (or (looking-at "\n\n")
                     (and (equal (1+ end) (point-max)) (looking-at "\n"))))
          (insert "\n"))
        ;; Insert PREFIX
        (goto-char beg)
        (beginning-of-line)
        (while (< (point-at-bol) end)
          (insert prefix)
          (setq end (+ (length prefix) end))
          (forward-line)))))

(defun notroff-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (notroff-block-region beg end "> "))

(defun notroff-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (notroff-pre-region (region-beginning) (region-end))
    (insert "    ")))

(defun notroff-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (notroff-block-region beg end "    "))

;;; Footnotes ======================================================================

(defun notroff-footnote-counter-inc ()
  "Increment notroff-footnote-counter and return the new value."
  (when (= notroff-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" notroff-footnote-chars "*?\\)\\]:")
				(point-max) t)
	(let ((fn (string-to-number (match-string 1))))
	  (when (> fn notroff-footnote-counter)
	    (setq notroff-footnote-counter fn))))))
  (incf notroff-footnote-counter))

(defun notroff-footnote-new ()
  "Insert a footnote with a new number and jump to a position to enter the
footnote text."
  (interactive)
  (let ((fn (notroff-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (notroff-footnote-text-find-new-location)
    (insert (format "[^%d]: " fn))))

(defun notroff-footnote-text-find-new-location ()
  "Position the cursor at the proper location for a new footnote text."
  (cond
   ((eq notroff-footnote-location 'end) (goto-char (point-max)))
   ((eq notroff-footnote-location 'immediately) (forward-paragraph))
   ((eq notroff-footnote-location 'header)
    ;; search for a header. if none is found, go to the end of the document.
    (catch 'eof
      (while (progn
	       (forward-paragraph)
	       (unless (re-search-forward notroff-regex-header nil t)
		 (throw 'eof nil))
	       (backward-paragraph)
	       (not (looking-at (concat "\n" notroff-regex-header))))))))
  ;; make sure we're on an empty line:
  (unless (notroff-cur-line-blank-p)
    (insert "\n"))
  ;; and make sure the previous line is empty:
  (unless (notroff-prev-line-blank-p)
    (insert "\n"))
  ;; then make sure there's an empty line following the footnote:
  (unless (notroff-next-line-blank-p)
    (insert "\n")
    (forward-line -1)))

(defun notroff-footnote-goto-text ()
  "Jump to the text of the footnote under the cursor."
  (interactive)
  ;; first make sure we're at a footnote marker
  (unless (or (looking-back (concat "\\[\\^" notroff-footnote-chars "*\\]?") (point-at-bol))
	      (looking-at (concat "\\[?\\^" notroff-footnote-chars "*?\\]")))
    (error "Not at a footnote"))
  (let* ((fn nil)
	 (new-pos (save-excursion
		    ;; move point between [ and ^:
		    (if (looking-at "\\[")
			(forward-char 1)
		      (skip-chars-backward "^["))
		    (looking-at (concat "\\(\\^" notroff-footnote-chars "*?\\)\\]"))
		    (setq fn (match-string 1))
		    (goto-char (point-min))
		    (re-search-forward (concat "^\\[" fn "\\]:") nil t))))
    (unless new-pos
      (error "No definition found for footnote `%s'" fn))
    (goto-char new-pos)
    (skip-chars-forward "[:space:]")))

(defun notroff-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
	      (backward-paragraph)
	      ;; if we're in a multiparagraph footnote, we need to back up further
	      (while (>= (notroff-next-line-indent) 4)
		(backward-paragraph))
	      (forward-line)
	      (if (looking-at (concat "^\\[\\(\\^" notroff-footnote-chars "*?\\)\\]:"))
		  (match-string 1)))))
    (unless fn
      (error "Not in a footnote"))
    (let ((new-pos (save-excursion
		     (goto-char (point-min))
		     (re-search-forward (concat "\\[" fn "\\]\\([^:]\\|\\'\\)") nil t))))
      (unless new-pos
	(error "Footnote `%s' not found" fn))
      (goto-char new-pos)
      (skip-chars-backward "^]"))))

;;; Indentation ====================================================================

(defun notroff-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun notroff-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `notroff-enter-key' or
`notroff-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `notroff-enter-key', by an initial call of
`notroff-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position."
  (interactive)
  (let ((positions (notroff-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'notroff-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (notroff-indent-find-next-position cur-pos positions)))))

(defun notroff-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level."
  (let (pos prev-line-pos positions)

    ;; Previous line indent
    (setq prev-line-pos (notroff-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq pos (notroff-prev-non-list-indent))
    (when pos
        (setq positions (cons pos positions))
        (setq positions (cons (+ pos tab-width) positions)))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward notroff-regex-list-indent (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if (and pos (not (eq pos prev-line-pos)))
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    (reverse positions)))

(defun notroff-do-normal-return ()
  "Insert a newline and optionally indent the next line."
  (newline)
  (if notroff-indent-on-enter
      (funcall indent-line-function)))

(defun notroff-enter-key ()
  "Handle RET according to context.
If there is a wiki link at the point, follow it unless
`notroff-follow-wiki-link-on-enter' is nil.  Otherwise, process
it in the usual way."
  (interactive)
  (if (and notroff-follow-wiki-link-on-enter (notroff-wiki-link-p))
      (notroff-follow-wiki-link-at-point)
    (notroff-do-normal-return)))



;;; Keymap ====================================================================

(defvar notroff-mode-map
  (let ((map (make-keymap)))
    ;; Element insertion
    (define-key map "\C-c\C-al" 'notroff-insert-link)
    (define-key map "\C-c\C-ar" 'notroff-insert-reference-link-dwim)
    (define-key map "\C-c\C-aw" 'notroff-insert-wiki-link)
    (define-key map "\C-c\C-ii" 'notroff-insert-image)
    (define-key map "\C-c\C-t1" 'notroff-insert-header-1)
    (define-key map "\C-c\C-t2" 'notroff-insert-header-2)
    (define-key map "\C-c\C-t3" 'notroff-insert-header-3)
    (define-key map "\C-c\C-t4" 'notroff-insert-header-4)
    (define-key map "\C-c\C-t5" 'notroff-insert-header-5)
    (define-key map "\C-c\C-t6" 'notroff-insert-header-6)
    (define-key map "\C-c\C-pb" 'notroff-insert-bold)
    (define-key map "\C-c\C-ss" 'notroff-insert-bold)
    (define-key map "\C-c\C-pi" 'notroff-insert-italic)
    (define-key map "\C-c\C-se" 'notroff-insert-italic)
    (define-key map "\C-c\C-pf" 'notroff-insert-code)
    (define-key map "\C-c\C-sc" 'notroff-insert-code)
    (define-key map "\C-c\C-sb" 'notroff-insert-blockquote)
    (define-key map "\C-c\C-s\C-b" 'notroff-blockquote-region)
    (define-key map "\C-c\C-sp" 'notroff-insert-pre)
    (define-key map "\C-c\C-s\C-p" 'notroff-pre-region)
    (define-key map "\C-c-" 'notroff-insert-hr)
    (define-key map "\C-c\C-tt" 'notroff-insert-title)
    (define-key map "\C-c\C-ts" 'notroff-insert-section)
    ;; Footnotes
    (define-key map "\C-c\C-fn" 'notroff-footnote-new)
    (define-key map "\C-c\C-fg" 'notroff-footnote-goto-text)
    (define-key map "\C-c\C-fb" 'notroff-footnote-return)
    ;; WikiLink Following
    (define-key map "\C-c\C-w" 'notroff-follow-wiki-link-at-point)
    (define-key map "\M-n" 'notroff-next-wiki-link)
    (define-key map "\M-p" 'notroff-previous-wiki-link)
    ;; Indentation
    (define-key map "\C-m" 'notroff-enter-key)
    ;; Visibility cycling
    (define-key map (kbd "<tab>") 'notroff-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'notroff-shifttab)
    ;; Header navigation
    (define-key map (kbd "C-M-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-M-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-M-f") 'outline-forward-same-level)
    (define-key map (kbd "C-M-b") 'outline-backward-same-level)
    (define-key map (kbd "C-M-u") 'outline-up-heading)
    ;; Notroff functions
    (define-key map "\C-c\C-cm" 'notroff)
    (define-key map "\C-c\C-cp" 'notroff-preview)
    (define-key map "\C-c\C-ce" 'notroff-export)
    (define-key map "\C-c\C-cv" 'notroff-export-and-view)
    ;; References
    (define-key map "\C-c\C-cc" 'notroff-check-refs)
    map)
  "Keymap for Notroff major mode.")

;;; Menu ==================================================================

(easy-menu-define notroff-mode-menu notroff-mode-map
  "Menu for Notroff mode"
  '("Notroff"
    ("Show/Hide"
     ["Cycle visibility" notroff-cycle (outline-on-heading-p)]
     ["Cycle global visibility" notroff-shifttab])
    "---"
    ["Compile" notroff]
    ["Preview" notroff-preview]
    ["Export" notroff-export]
    ["Export & View" notroff-export-and-view]
    "---"
    ("Headers (setext)"
     ["Insert Title" notroff-insert-title]
     ["Insert Section" notroff-insert-section])
    ("Headers (atx)"
     ["First level" notroff-insert-header-1]
     ["Second level" notroff-insert-header-2]
     ["Third level" notroff-insert-header-3]
     ["Fourth level" notroff-insert-header-4]
     ["Fifth level" notroff-insert-header-5]
     ["Sixth level" notroff-insert-header-6])
    "---"
    ["Bold" notroff-insert-bold]
    ["Italic" notroff-insert-italic]
    ["Blockquote" notroff-insert-blockquote]
    ["Preformatted" notroff-insert-pre]
    ["Code" notroff-insert-code]
    "---"
    ["Insert inline link" notroff-insert-link]
    ["Insert reference link" notroff-insert-reference-link-dwim]
    ["Insert image" notroff-insert-image]
    ["Insert horizontal rule" notroff-insert-hr]
    "---"
    ("Footnotes"
     ["Insert footnote" notroff-footnote-new]
     ["Jump to footnote text" notroff-footnote-goto-text]
     ["Return from footnote" notroff-footnote-return])
    "---"
    ["Check references" notroff-check-refs]
    "---"
    ["Version" notroff-show-version]
    ))



;;; References ================================================================

;;; Undefined reference checking code by Dmitry Dzhus <mail@sphinx.net.ru>.

(defconst notroff-refcheck-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`notroff-mode' buffer name.")

(defun notroff-has-reference-definition (reference)
    "Find out whether Notroff REFERENCE is defined.

REFERENCE should include the square brackets, like [this]."
    (let ((reference (downcase reference)))
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (while (re-search-forward notroff-regex-reference-definition nil t)
            (when (string= reference (downcase (match-string-no-properties 1)))
              (throw 'found t)))))))

(defun notroff-get-undefined-refs ()
  "Return a list of undefined Notroff references.

Result is an alist of pairs (reference . occurencies), where
occurencies is itself another alist of pairs (label .
line-number).

For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"[emacs]\" (\"[Nice editor]\" . 12) (\"[GNU Emacs]\" . 45)) (\"[elisp]\" (\"[manual]\" . 127)))."
  (let ((missing))
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward notroff-regex-link-reference nil t)
        (let* ((label (match-string-no-properties 1))
               (reference (match-string-no-properties 2))
               (target (downcase (if (string= reference "[]") label reference))))
          (unless (notroff-has-reference-definition target)
            (let ((entry (assoc target missing)))
              (if (not entry)
                  (add-to-list 'missing (cons target
                                              (list (cons label (notroff-line-number-at-pos)))) t)
                (setcdr entry
                        (append (cdr entry) (list (cons label (notroff-line-number-at-pos))))))))))
      missing)))

(defun notroff-add-missing-ref-definition (ref buffer &optional recheck)
  "Add blank REF definition to the end of BUFFER.

REF is a Notroff reference in square brackets, like \"[lisp-history]\".

When RECHECK is non-nil, BUFFER gets rechecked for undefined
references so that REF disappears from the list of those links."
  (with-current-buffer buffer
      (when (not (eq major-mode 'notroff-mode))
        (error "Not available in current mode"))
      (goto-char (point-max))
      (indent-new-comment-line)
      (insert (concat ref ": ")))
  (switch-to-buffer-other-window buffer)
  (goto-char (point-max))
  (when recheck
    (notroff-check-refs t)))

;; Button which adds an empty Notroff reference definition to the end
;; of buffer specified as its 'target-buffer property. Reference name
;; is button's label
(when (>= emacs-major-version 22)
  (define-button-type 'notroff-ref-button
    'help-echo "Push to create an empty reference definition"
    'face 'bold
    'action (lambda (b)
              (notroff-add-missing-ref-definition
               (button-label b) (button-get b 'target-buffer) t))))

;; Button jumping to line in buffer specified as its 'target-buffer
;; property. Line number is button's 'line property.
(when (>= emacs-major-version 22)
  (define-button-type 'goto-line-button
    'help-echo "Push to go to this line"
    'face 'italic
    'action (lambda (b)
              (message (button-get b 'buffer))
              (switch-to-buffer-other-window (button-get b 'target-buffer))
              ;; use call-interactively to silence compiler
              (call-interactively 'goto-line (button-get b 'target-line)))))

(defun notroff-check-refs (&optional silent)
  "Show all undefined Notroff references in current `notroff-mode' buffer.

If SILENT is non-nil, do not message anything when no undefined
references found.

Links which have empty reference definitions are considered to be
defined."
  (interactive "P")
  (when (not (eq major-mode 'notroff-mode))
    (error "Not available in current mode"))
  (let ((oldbuf (current-buffer))
        (refs (notroff-get-undefined-refs))
        (refbuf (get-buffer-create (notroff-replace-regexp-in-string
                                 "%buffer%" (buffer-name)
                                 notroff-refcheck-buffer))))
    (if (null refs)
        (progn
          (when (not silent)
            (message "No undefined references found"))
          (kill-buffer refbuf))
      (with-current-buffer refbuf
        (when view-mode
          (View-exit-and-edit))
        (erase-buffer)
        (insert "Following references lack definitions:")
        (newline 2)
        (dolist (ref refs)
          (let ((button-label (format "%s" (car ref))))
            (if (>= emacs-major-version 22)
                ;; Create a reference button in Emacs 22
                (insert-text-button button-label
                                    :type 'notroff-ref-button
                                    'target-buffer oldbuf)
              ;; Insert reference as text in Emacs < 22
              (insert button-label)))
          (insert " (")
          (dolist (occurency (cdr ref))
            (let ((line (cdr occurency)))
              (if (>= emacs-major-version 22)
                  ;; Create a line number button in Emacs 22
                  (insert-button (number-to-string line)
                                 :type 'goto-line-button
                                 'target-buffer oldbuf
                                 'target-line line)
                ;; Insert line number as text in Emacs < 22
                (insert (number-to-string line)))
              (insert " "))) (delete-char -1)
          (insert ")")
          (newline))
        (view-buffer-other-window refbuf)
        (goto-char (point-min))
        (forward-line 2)))))

;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for notroff-mode.

(defvar notroff-cycle-global-status 1)
(defvar notroff-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun notroff-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun notroff-cycle (&optional arg)
  "Visibility cycling for Notroff mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
     ((eq arg t) ;; Global cycling
      (cond
       ((and (eq last-command this-command)
             (eq notroff-cycle-global-status 2))
        ;; Move from overview to contents
        (hide-sublevels 1)
        (message "CONTENTS")
        (setq notroff-cycle-global-status 3))

       ((and (eq last-command this-command)
             (eq notroff-cycle-global-status 3))
        ;; Move from contents to all
        (show-all)
        (message "SHOW ALL")
        (setq notroff-cycle-global-status 1))

       (t
        ;; Defaults to overview
        (hide-body)
        (message "OVERVIEW")
        (setq notroff-cycle-global-status 2))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (outline-back-to-heading)
          (save-excursion
            (beginning-of-line 2)
            (while (and (not (eobp)) ;; this is like `next-line'
                        (get-char-property (1- (point)) 'invisible))
              (beginning-of-line 2)) (setq eol (point)))
          (outline-end-of-heading)   (setq eoh (point))
          (notroff-end-of-subtree t)
          (skip-chars-forward " \t\n")
          (beginning-of-line 1) ; in case this is an item
          (setq eos (1- (point))))
        ;; Find out what to do next and set `this-command'
      (cond
         ((= eos eoh)
          ;; Nothing is hidden behind this heading
          (message "EMPTY ENTRY")
          (setq notroff-cycle-subtree-status nil))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line: open it
          (show-entry)
          (show-children)
          (message "CHILDREN")
          (setq notroff-cycle-subtree-status 'children))
         ((and (eq last-command this-command)
               (eq notroff-cycle-subtree-status 'children))
          ;; We just showed the children, now show everything.
          (show-subtree)
          (message "SUBTREE")
          (setq notroff-cycle-subtree-status 'subtree))
         (t
          ;; Default action: hide the subtree.
          (hide-subtree)
          (message "FOLDED")
          (setq notroff-cycle-subtree-status 'folded)))))

     (t
      (indent-for-tab-command))))

;; Based on org-shifttab from org.el.
(defun notroff-shifttab ()
  "Global visibility cycling.
Calls `notroff-cycle' with argument t."
  (interactive)
  (notroff-cycle t))

(defun notroff-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((match-end 1) 1)
   ((match-end 2) 2)
   ((- (match-end 0) (match-beginning 0)))))

;;; Commands ==================================================================

(defun notroff (&optional output-buffer-name)
  "Run `notroff' on current buffer and insert output in buffer BUFFER-OUTPUT."
  (interactive)
  (let ((title (buffer-name))
        (begin-region)
        (end-region))
    (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
        (setq begin-region (region-beginning)
              end-region (region-end))
      (setq begin-region (point-min)
            end-region (point-max)))

    (unless output-buffer-name
      (setq output-buffer-name notroff-output-buffer-name))

    (if notroff-command-needs-filename
        ;; Handle case when `notroff-command' does not read from stdin
        (if (not buffer-file-name)
            (error "Must be visiting a file")
          (shell-command (concat notroff-command " "
                                 (shell-quote-argument buffer-file-name))
                         output-buffer-name))
      ;; Pass region to `notroff-command' via stdin
      (shell-command-on-region begin-region end-region notroff-command
                               output-buffer-name))

    ;; Add header and footer and switch to html-mode.
    (save-current-buffer
      (set-buffer output-buffer-name)
      (goto-char (point-min))
      (unless (notroff-output-standalone-p)
        (notroff-add-xhtml-header-and-footer title))
      (html-mode))

    ;; Ensure buffer gets raised, even with short command output
    (switch-to-buffer-other-window output-buffer-name)))

(defun notroff-output-standalone-p ()
  "Determine whether `notroff-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`notroff-xhtml-standalone-regexp' in the first five lines of output."
  (re-search-forward
   notroff-xhtml-standalone-regexp
   (save-excursion (goto-char (point-min)) (forward-line 4) (point))
   t))

(defun notroff-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (if (> (length notroff-css-path) 0)
      (insert "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
              notroff-css-path
              "\"  />\n"))
  (when (> (length notroff-xhtml-header-content) 0)
    (insert notroff-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (goto-char (point-max))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun notroff-preview ()
  "Run `notroff' on the current buffer and preview the output in a browser."
  (interactive)
  (notroff notroff-output-buffer-name)
  (browse-url-of-buffer notroff-output-buffer-name))

(defun notroff-export-file-name ()
  "Attempt to generate a filename for Notroff output.
If the current buffer is visiting a file, we construct a new
output filename based on that filename.  Otherwise, return nil."
  (when (buffer-file-name)
    (concat (file-name-sans-extension (buffer-file-name)) ".html")))

(defun notroff-export ()
  "Run Notroff on the current buffer, save to a file, and return the filename.
The resulting filename will be constructed using the current filename, but
with the extension removed and replaced with .html."
  (interactive)
  (let ((output-file (notroff-export-file-name))
        (output-buffer-name))
    (when output-file
      (setq output-buffer-name (buffer-name (find-file-noselect output-file)))
      (notroff output-buffer-name)
      (with-current-buffer output-buffer-name
        (save-buffer)
        (kill-buffer-and-window))
      output-file)))

(defun notroff-export-and-view ()
  "Export to XHTML using `notroff-export' and browse the resulting file."
  (interactive)
  (browse-url (notroff-export)))

;;; WikiLink Following/Markup =================================================

(require 'thingatpt)

(defun notroff-wiki-link-p ()
  "Return non-nil when `point' is at a true wiki link.
A true wiki link name matches `notroff-regex-wiki-link' but does not
match the current file name after conversion.  This modifies the data
returned by `match-data'.  Note that the potential wiki link name must
be available via `match-string'."
  (let ((case-fold-search nil))
    (and (thing-at-point-looking-at notroff-regex-wiki-link)
	 (or (not buffer-file-name)
	     (not (string-equal (buffer-file-name)
				(notroff-convert-wiki-link-to-filename
                                 (notroff-wiki-link-link)))))
	 (not (save-match-data
		(save-excursion))))))

(defun notroff-wiki-link-link ()
  "Return the link part of the wiki link using current match data.
The location of the link component depends on the value of
`notroff-wiki-link-alias-first'."
  (if notroff-wiki-link-alias-first
      (or (match-string 3) (match-string 1))
    (match-string 1)))

(defun notroff-convert-wiki-link-to-filename (name)
  "Generate a filename from the wiki link NAME.
Spaces in NAME are replaced with `notroff-link-space-sub-char'.
When in `gfm-mode', follow GitHub's conventions where [[Test Test]]
and [[test test]] both map to Test-test.ext."
  (let ((basename (notroff-replace-regexp-in-string
                   "[[:space:]\n]" notroff-link-space-sub-char name)))
    (when (eq major-mode 'gfm-mode)
      (setq basename (concat (upcase (substring basename 0 1))
                             (downcase (substring basename 1 nil)))))
    (concat basename
            (if (buffer-file-name)
                (concat "."
                        (file-name-extension (buffer-file-name)))))))

(defun notroff-follow-wiki-link (name)
  "Follow the wiki link NAME.
Convert the name to a file name and call `find-file'.  Ensure that
the new buffer remains in `notroff-mode'."
  (let ((filename (notroff-convert-wiki-link-to-filename name)))
    (find-file filename))
  (notroff-mode))

(defun notroff-follow-wiki-link-at-point ()
  "Find Wiki Link at point.
See `notroff-wiki-link-p' and `notroff-follow-wiki-link'."
  (interactive)
  (if (notroff-wiki-link-p)
      (notroff-follow-wiki-link (notroff-wiki-link-link))
    (error "Point is not at a Wiki Link")))

(defun notroff-next-wiki-link ()
  "Jump to next wiki link.
See `notroff-wiki-link-p'."
  (interactive)
  (if (notroff-wiki-link-p)
      ; At a wiki link already, move past it.
      (goto-char (+ 1 (match-end 0))))
  (save-match-data
    ; Search for the next wiki link and move to the beginning.
    (re-search-forward notroff-regex-wiki-link nil t)
    (goto-char (match-beginning 0))))

(defun notroff-previous-wiki-link ()
  "Jump to previous wiki link.
See `notroff-wiki-link-p'."
  (interactive)
  (re-search-backward notroff-regex-wiki-link nil t))

(defun notroff-highlight-wiki-link (from to face)
  "Highlight the wiki link in the region between FROM and TO using FACE."
  (put-text-property from to 'font-lock-face face))

(defun notroff-unfontify-region-wiki-links (from to)
  "Remove wiki link faces from the region specified by FROM and TO."
  (interactive "nfrom: \nnto: ")
  (remove-text-properties from to '(font-lock-face notroff-link-face))
  (remove-text-properties from to '(font-lock-face notroff-missing-link-face)))

(defun notroff-fontify-region-wiki-links (from to)
  "Search region given by FROM and TO for wiki links and fontify them.
If a wiki link is found check to see if the backing file exists
and highlight accordingly."
  (goto-char from)
  (while (re-search-forward notroff-regex-wiki-link to t)
    (let ((highlight-beginning (match-beginning 0))
	  (highlight-end (match-end 0))
	  (file-name
	   (notroff-convert-wiki-link-to-filename
            (notroff-wiki-link-link))))
      (if (file-exists-p file-name)
	  (notroff-highlight-wiki-link
	   highlight-beginning highlight-end notroff-link-face)
	(notroff-highlight-wiki-link
	 highlight-beginning highlight-end notroff-missing-link-face)))))

(defun notroff-extend-changed-region (from to)
  "Extend region given by FROM and TO so that we can fontify all links.
The region is extended to the first newline before and the first
newline after."
  ;; start looking for the first new line before 'from
  (goto-char from)
  (re-search-backward "\n" nil t)
  (let ((new-from (point-min))
	(new-to (point-max)))
    (if (not (= (point) from))
	(setq new-from (point)))
    ;; do the same thing for the first new line after 'to
    (goto-char to)
    (re-search-forward "\n" nil t)
    (if (not (= (point) to))
	(setq new-to (point)))
    (list new-from new-to)))

(defun notroff-check-change-for-wiki-link (from to change)
  "Check region between FROM and TO for wiki links and re-fontfy as needed.
Designed to be used with the `after-change-functions' hook.
CHANGE is the number of bytes of pre-change text replaced by the
given range."
  (interactive "nfrom: \nnto: \nnchange: ")
  (let* ((inhibit-quit t)
	 (modified (buffer-modified-p))
	 (buffer-undo-list t)
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 (current-point (point))
	 deactivate-mark)
    (unwind-protect
        (save-match-data
          (save-restriction
            ;; Extend the region to fontify so that it starts
            ;; and ends at safe places.
            (multiple-value-bind (new-from new-to)
                (notroff-extend-changed-region from to)
              ;; Unfontify existing fontification (start from scratch)
              (notroff-unfontify-region-wiki-links new-from new-to)
              ;; Now do the fontification.
              (notroff-fontify-region-wiki-links new-from new-to)))
          (unless modified
            (if (fboundp 'restore-buffer-modified-p)
                (restore-buffer-modified-p nil)
              (set-buffer-modified-p nil))))
      (goto-char current-point))))

(defun notroff-fontify-buffer-wiki-links ()
  "Refontify all wiki links in the buffer."
  (interactive)
  (notroff-check-change-for-wiki-link (point-min) (point-max) 0))

;;; Miscellaneous =============================================================

(defun notroff-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun notroff-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))



;;; Mode definition  ==========================================================

(defun notroff-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "notroff-mode, version %s" notroff-mode-version))

;;;###autoload
(define-derived-mode notroff-mode text-mode "Notroff"
  "Major mode for editing Notroff files."
  ;; Natural Notroff tab width
  (setq tab-width 4)
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(notroff-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For menu support in XEmacs
  (easy-menu-add notroff-mode-menu notroff-mode-map)
  ;; Make filling work with lists (unordered, ordered, and definition)
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t]*[0-9]+\\.\\|^[ \t]*: ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp notroff-regex-header)
  (make-local-variable 'outline-level)
  (setq outline-level 'notroff-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'notroff-nobreak-p)
  (setq indent-line-function notroff-indent-function)

  ;; Prepare hooks for XEmacs compatibility
  (when (featurep 'xemacs)
      (make-local-hook 'after-change-functions)
      (make-local-hook 'font-lock-extend-region-functions)
      (make-local-hook 'window-configuration-change-hook))

  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'notroff-font-lock-extend-region)

  ;; Anytime text changes make sure it gets fontified correctly
  (add-hook 'after-change-functions 'notroff-check-change-for-wiki-link t t)

  ;; If we left the buffer there is a really good chance we were
  ;; creating one of the wiki link documents. Make sure we get
  ;; refontified when we come back.
  (add-hook 'window-configuration-change-hook
	    'notroff-fontify-buffer-wiki-links t t)

  ;; do the initial link fontification
  (notroff-fontify-buffer-wiki-links))

;(add-to-list 'auto-mode-alist '("\\.text$" . notroff-mode))

;;; GitHub Flavored Notroff Mode  ============================================

(define-derived-mode gfm-mode notroff-mode "GFM"
  "Major mode for editing GitHub Flavored Notroff files."
  (setq notroff-link-space-sub-char "-")
  (auto-fill-mode 0)
  ;; Use visual-line-mode if available, fall back to longlines-mode:
  (if (fboundp 'visual-line-mode)
      (visual-line-mode 1)
    (longlines-mode 1))
  ;; do the initial link fontification
  (notroff-fontify-buffer-wiki-links))


(provide 'notroff-mode)

;;; notroff-mode.el ends here
