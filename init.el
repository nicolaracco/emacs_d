;; Before everything else, keep it real
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;
;; This is the original stuff
;;
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; this is so important that it goes here
(setq-default indent-tabs-mode nil)

;; sets the default font to menlo
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Menlo Regular-14"))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; default window size
(setq default-frame-alist '((width . 105) (height . 38) ))

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(coffee-tab-width 2)
 '(c-basic-offset 2))

;; activates forward delete on del key
(global-set-key [kp-delete] 'delete-char)

;; remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; this is included
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq auto-mode-alist  (cons '("Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Gemfile.lock$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Rakefile$" . ruby-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Buildfile$" . ruby-mode))

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs_backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; hack until I can make it compile via el-get
(add-to-list 'load-path "~/.emacs.d/vendor/slime")  ; your SLIME directory
(setq inferior-lisp-program "/usr/local/bin/ccl") ; your Lisp system
(require 'slime)
(slime-setup '(slime-fancy))

;; Apply shell environment to emacs
(require 'cl)
(defun env-line-to-cons (env-line)
  "Convert a string of the form \"VAR=VAL\" to a
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
      (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "Launch /usr/bin/env or the equivalent from a login
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "$SHELL -lc")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

(defun setenv-from-cons (var-val)
  "Set an environment variable from a cons cell containing
two strings, where the car is the variable name and cdr is
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "Apply the environment reported by `/usr/bin/env' (or env-cmd)
as launched by `$SHELL -lc' (or shell-cmd) to the current
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

(setenv-from-shell-environment)
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Here I start configuring it with el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; MOAR PACKAGES!!!
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq el-get-sources
      '((:name color-theme-zenburn
               :type git
               :url "https://github.com/bbatsov/zenburn-emacs.git"
               :load ("color-theme.el" "color-theme-zenburn.el"))
        (:name coffee-mode
               :website "http://ozmm.org/posts/coffee_mode.html"
               :description "Emacs Major Mode for CoffeeScript"
               :type git
               :url "https://github.com/intinig/coffee-mode.git"
               :features coffee-mode
               :post-init (lambda ()
                            (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
                            (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
                            (define-key coffee-mode-map (kbd "C-x t") 'coffee-compile-buffer)))
        (:name scala-mode
               :type svn
               :url "http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs/"
               :build ("ELISP_COMMAND=/Applications/Emacs.app/Contents/MacOS/Emacs make")
               :load-path (".")
               :features scala-mode-auto)
        (:name linum-ex
               :type git
               :url "git://github.com/nicolaracco/linum-ex.git"
               :features linum-ex
               :post-init (lambda()
                            (global-linum-mode t)
                            (global-set-key (kbd "C-<f5>") 'linum-mode)))))

(setq el-get-packages
      (append
       '(apache-mode
         autopair
         haml-mode
         gist
         inf-ruby
         js-comint
         js2-mode
         mustache-mode
         org-mode
         php-mode-improved
         rhtml-mode
	 ruby-compilation
         rvm
         sass-mode
	 smooth-scrolling
         textmate
	 twittering-mode
	 yasnippet
	 yaml-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync el-get-packages)

;; Various after-el-get-configurations
(defun hash-arrow () (interactive) (insert " => "))
(global-set-key (kbd "C-l") 'hash-arrow)
(setq org-log-done t)
(color-theme-zenburn)
(column-number-mode)
(ansi-color-for-comint-mode-on)
(show-paren-mode 1)
(setq twittering-use-master-password t)
(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))
(autopair-global-mode)
(rvm-use-default)
(global-set-key (kbd "C-<f5>") 'linum-mode)
(global-linum-mode t)
(delete-selection-mode 1)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-hook 'coffee-mode-hook '(lambda ()
                               (and (file-exists-p (buffer-file-name))
                                    (file-exists-p (coffee-compiled-file-name))
                                    (coffee-cos-mode t))))

(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;; uniquify lines in a buffer
;; thanks http://yesybl.org/blogen/?p=25
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))


;; Extra directives to keep this pristine.
(if
  (file-readable-p "~/.emacs.d/user.el")
  (load-file "~/.emacs.d/user.el")
)
