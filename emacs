;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq column-number-mode t)  ; show column numbers
(setq explicit-shell-filename "/bin/bash")  ; use bash for shell buffers
(setq inhibit-startup-screen t)  ; skip startup screen
(menu-bar-mode 0)  ; no menubar
(tool-bar-mode 0)  ; no toolbar

;(load "~/.emacs.d/themes/meacupla-theme.el")
;(set-default-font "Inconsolata 15")
(set-frame-font "Source Code Pro 14")

;; C-x b to a buffer should always show the buffer in the current window,
;; even if open somewhere else -- sometimes we want two windows on the
;; buffer.
;;(setq display-buffer-overriding-action '(display-buffer-same-window . nil))

;;(evil-mode)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From https://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs:
(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
     "Keymap for local bindings and functions, prefixed by (^L)")

(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

(define-key ctl-l-map "\C-l" 'other-window)

(define-key ctl-l-map "h" 'windmove-left)
(define-key ctl-l-map "l" 'windmove-right)
(define-key ctl-l-map "k" 'windmove-up)
(define-key ctl-l-map "j" 'windmove-down)

;;(global-set-key "\C-xo" 'switch-window)
(define-key ctl-l-map "\C-g" 'switch-window)

(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

(global-set-key (kbd "M-.") 'meta-dot-key)
(defun meta-dot-key ()
  (interactive)
  (stacknav-mark)
  (lsp-goto-type-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-list
	'(flycheck  ; on-the-fly error checking
	  company  ; autocompletions
	  lsp-mode ; language server protocol
	  ; Rust...
	  rust-mode
	  flycheck-rust
	  ; C++...
	  clang-format
	  ycmd
	  company-ycmd
	  ; Other languages:
	  csharp-mode
	  scala-mode
	  haskell-mode
	  protobuf-mode
	  gnuplot-mode
	  lua-mode
	  toml-mode
	  geiser
	  slime
	  ; File type bindings:
	  openwith
	  ; Editing modes and additions:
	  evil
	  undo-tree
	  ;redo+
          key-chord
	  ; Window movement helper:
	  switch-window
	  color-theme-modern
      ; Helm and projectile:
      projectile
      helm
      helm-projectile))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;(load-theme 'gnome2 t t)
;;(enable-theme 'gnome2)
;;(color-theme-initialize)
;;(color-theme-darktty)

(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)

;;(key-chord-mode)
;;(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq openwith-associations
      '(("\\.pdf\\'" "qpdfview" (file))
	("\\.svg\\'" "inkscape" (file))))
(openwith-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Org-Mode (http://orgmode.org/manual/Activation.html#Activation)
(setq org-agenda-files '("~/notes/work.org"))
(setq org-file-apps
      '(("\\.pdf\\'" . "qpdfview %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C, Python and indenting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq py-indent-offset 4)
(setq c-basic-offset 4)
(setq c-default-style "bsd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)
(require 'flycheck-rust)

(add-hook 'rust-mode-hook (lambda ()
			    (flycheck-mode)
			    (company-mode)
                            (lsp)
                            (local-set-key (kbd "C-c C-i") #'rustfmt-format-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clang-format)

(add-hook 'c++-mode-hook (lambda ()
			   (local-set-key (kbd "\C-q") #'clang-format-region)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datalog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/souffle-mode.el")

(add-to-list 'auto-mode-alist '("\\.dl\\'" . souffle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company (autocompletions).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq company-tooltip-align-annotations t)
(setq company-idle-delay .3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'latex-mode-hook (lambda ()
			     (flyspell-mode)
			     (reftex-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/stacknav.el")

; http://everything2.com/index.pl?node_id=1038451
(defun create-scratch-buffer nil
  (interactive)
  (let ((n 0)
        (bufname ""))
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (if (= n 1) (lisp-interaction-mode))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map (kbd "M-S-q") 'unfill-paragraph)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode vterm unfill undo-tree treemacs-projectile toml-mode switch-window solarized-theme scala-mode rustfmt rfringe redo+ racket-mode racer protobuf-mode projectile-ripgrep openwith ocamlformat nasm-mode merlin magit lua-mode lsp-ivy key-chord helm-projectile haskell-mode gruvbox-theme groovy-mode golden-ratio gnuplot-mode ggtags geiser-guile fsharp-mode flymake-rust flycheck-rust exec-path-from-shell evil deadgrep csharp-mode counsel company-ycmd company-racer color-theme-modern color-theme clang-format cargo caml bind-key ac-slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
