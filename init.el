;; Additional configuration outside of .emacs.d

(add-to-list 'load-path "~/.emacs.d.jude/customizations")

;; org-mode
(load "my-org.el")

;; Set the theme depending on time of day
(load "theme-changer.el")
(require 'theme-changer)
(change-theme 'solarized-light 'solarized-dark)

(windmove-default-keybindings)
;; hide the toolbar
(tool-bar-mode -1)

;; disable the scrollbar
(scroll-bar-mode -1)


;; set the font
(add-to-list 'default-frame-alist '(font . "Fira Code"))
;;(setq-default line-spacing 0)

;; stuff needed to get fira code ligatures to work
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(set-face-attribute 'default nil :height 140)

;; Set the default directory
(setq default-directory "~")

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Allow hash to be entered  
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; set registers for commonly accessed files
;; access these with C-x r j <key>
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?c '(file . "~/Dropbox/Projects/clojure/clojure-cheatsheet.txt"))
(set-register ?e '(file . "~/Dropbox/Projects/emacs.notes"))


;; Don't pop up a Cider erro buffer - show errors in repl
(setq cider-show-error-buffer '(only-in-repl))
