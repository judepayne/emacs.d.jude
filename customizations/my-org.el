;; Org mode stuff

;; QUICK NOTES ON ORG-MODE FOR CLOJURE (stuff I have found useful)
;;
;; 1.) Proper evaluation of clojure code blocks
;; Once all the setup is done and you have written an .org file with embedded
;; clojure code blocks, you have to cider-jack-in - then before any of the
;; blocks will evaluate properly, in the cider-repl buffer, you must switch to
;; the namespace of your org file - otherwise the blocks won't properly evaluate.
;; 2.) Occasionally the way this file works seems a little weird. If I load the file
;; at startup, no issues but some of the variables lower down e.g. org-html-scripts
;; don't seems to get set properly. it's set to nil at the top and then reset to what
;; I want lower down. Occasionally I need to go in and manually execute those blocks of
;; code in this file C-x C-e to ensure its set properly. Fairly minor problem - basically
;; works.

;; **************IN ORG MODE OPERATION  SETTINGS ******************

(require 'org)

;; We only need Emacs Lisp and Clojure
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))

;; Use cider as the clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;; set up easy template for clojure block. type <s then TAB
(add-to-list 'org-structure-template-alist
             '("s" "\n#+Name: \n#+BEGIN_SRC clojure :exports both :result value pp\n\n#+END_SRC"))

;; **************ORG MODE HTML EXPORT SETTINGS ******************

(setq org-html-html5-fancy t)
(setq org-html-doctype "html5")

;; suppress various default org export options
(setq org-html-head-include-default-style nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-author nil)
(setq org-html-validation-link nil)
(setq org-html-preamble nil)
(setq shift-select-mode nil)
(setq org-html-head nil)
(setq org-html-head-extra nil)
(setq org-html-scripts nil)

;;---- Useful functions for handling text from files---
;; ---------- DONT'T EDIT THIS SECTION----------

(defun script-it (s)
  "wrap a string in <script> tags"
  (concat "<script>" s "</script>"))

(defun style-it (s)
  "wrap a string in <style> tags"
  (concat "<style>" s "</style>"))

(defun link-css (path)
  "creates a string of an html stylesheet link"
  (concat
   "<link rel=\"stylesheet\" type=\"text/css\" href=\""
   path
   "\">"))

(defun link-js (path)
  (concat
   "<script src=\""
   path
   "\"></script>"))

(defun read-file-string (path)
  "Returns contents of a file as a string"
  (with-temp-buffer
    (insert-file-contents path) (buffer-string)))

(defun read-file-lines (path)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
   White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
			    (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun minify-file (path)
  "remove all the whitespace and newlines from the text in a file
   (seems to be the web way - although indecipherable!). returns string"
  (apply 'concat (mapcar (lambda (x)
			   (trim-string x))
			 (read-file-lines path))))

(defun minify-files (paths wrap-fn)
  (replace-regexp-in-string "\n" ""
			    (apply 'concat
				   (mapcar
				    (lambda (x)
				      (concat (funcall wrap-fn (minify-file  x)) "\n"))
				    paths))))

;;---- END OF Useful functions for handling text from files---

;; ---SECTION WHERE YOU DEFINE SCRIPTS AND CSS's TO BE INLINED---
;; define style path (css & javascript) (need full path)
(defun style-dir> (s)
  (concat "/Users/jude/Dropbox/Projects/style/" s))

;; 1. SCRIPTS
;; you should define a list of script files for your html
(setq script-files
      (list
       (style-dir> "highlight/highlight.pack.js")))

;; you should define a list of script strings for your html
(setq script-includes
      (list
       "<script>hljs.initHighlightingOnLoad();</script>"))

;; you should set t/nil whether the contents of the scripts files
;;  are to be inlined or just linked to
(setq inline-scripts? t)

;; you should define a list of css files
(setq my-css (list
	      (style-dir> "css/styles.css")
	      ;; (style-dir> "highlight/styles/ascetic.css")
	      ;; (style-dir> "highlight/styles/pojoaque.css")
	      (style-dir> "highlight/styles/github.css")
	      ))

;; you should set t/nil whether contents of css files should be inlined
(setq inline-styles? t)

;; ---END OF SECTION WHERE YOU DEFINE SCRIPTS AND CSS's TO BE INLINED---
;; ---------------------DONT' EDIT THIS SECTION------------------------

(setq org-html-scripts
      (apply 'concat
	     (cons
	      (if inline-scripts?
		  (minify-files script-files 'script-it)
		(apply 'concat (mapcar 'link-js script-files)))
	      script-includes)))

(setq org-html-head-extra
      (if inline-styles?
	  (minify-files my-css 'style-it)
	(apply 'concat (mapcar 'link-css my-css))))
;; -------------------------END OF SECTION------------------------

(defun directory-files-recursive (directory match maxdepth)
  "List files in DIRECTORY and in its sub-directories. 
   Return files that match the regular expression MATCH. Recurse only 
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond 
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))     
          ;; recurse only if necessary
          (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1))))
          (setq files-list (cons f files-list)))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun tangle-all ()
  "Tangle all the Org-mode files in the directory of the file of the current buffer
   recursively in child folders. Returns the list of tangled files"
  (mapcar (lambda (f)
            (when (not (file-directory-p f))
              (org-babel-tangle-file f)))
          (directory-files-recursive (file-name-directory (buffer-file-name)) "\\.org$" 20)))

;; Formats the html output for code blocks in such a way that highlight.js
;; can pick it up.
(defun org-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
	  (caption (org-export-get-caption src-block))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (and (org-element-property :name src-block)
				 (org-export-get-reference src-block info))))
		   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre><code class=\"example\"%s>\n%s</code></pre>" label code)
	(format
	 "<div class=\"org-src-container\">\n%s%s\n</div>"
	 (if (not caption) ""
	   (format "<label class=\"org-src-name\">%s</label>"
		   (org-export-data caption info)))
	 (format "\n<pre class=\"src src-%s\"%s>%s</pre>" lang label code))))))
