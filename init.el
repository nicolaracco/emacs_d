(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 

(add-to-list 'load-path "~/.emacs.d/vendor/")

(require 'textmate)
(require 'peepopen)
(textmate-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(if (window-system) 
    (progn
      (add-to-list 'load-path "~/.emacs.d/vendor/color-theme-6.6.0")
      (require 'color-theme)
      (eval-after-load "color-theme"
	'(progn
	   (color-theme-initialize)
	   (color-theme-solarized-light)))))

;; sets the default font to menlo
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Menlo Regular-14"))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; default window size 
(setq default-frame-alist '((width . 105) (height . 40) ))

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil))
(custom-set-faces
 '(linum ((t (:inherit (shadow default) :family "Inconsolata")))))

;; normal scrolling (1 line)
(require 'smooth-scrolling)

;; activates forward delete on del key
(global-set-key [kp-delete] 'delete-char)

(add-to-list 'load-path "~/.emacs.d/vendor/twittering-mode")
(require 'twittering-mode)
(setq twittering-use-master-password t)

(setq exec-path (cons "/usr/local/bin" exec-path))

(add-to-list 'load-path "~/.emacs.d/vendor/git-emacs")
(require 'git-emacs)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(require 'autopair)
(autopair-global-mode)

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

(require 'ruby-compilation)
(setq auto-mode-alist  (cons '("Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Gemfile.lock$" . ruby-mode) auto-mode-alist))

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

(require 'linum-off)
(global-linum-mode 1)

(require 'rvm)
(rvm-use-default)

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
