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

;; C-x b to a buffer should always show the buffer in the current window,
;; even if open somewhere else -- sometimes we want two windows on the
;; buffer.
(setq display-buffer-overriding-action '(display-buffer-same-window . nil))

;;(evil-mode)

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
	  color-theme-modern))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;(load-theme 'gnome2 t t)
;;(enable-theme 'gnome2)
;;(color-theme-initialize)
;;(color-theme-darktty)

(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)

(key-chord-mode)
;;(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

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
                            (local-set-key (kbd "C-i") #'rustfmt-format-buffer)))

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

(defun unwrap nil
  (interactive)

  (let ((p (point))
        (start (point))
        (end (mark)))
    (when end
      ;; ensure start < end, swap if not
      (when (> start end)
        (let ((tmp start))
          (setq start end)
          (setq end tmp)))

      ;; go to the end of the first line
      (goto-char start)
      (end-of-line)
      ;; for each line until end, delete the newline, insert a space, and
      ;; move to the next end-of-line.
      (defun innerloop nil
        (when (< (point) end)
	  (cond
	   ;; if at end of paragraph, skip to the next para and loop.
	   ((and
	     (< (point) (- end 2))
	     (= (char-after (point)) 10)
	     (= (char-after (+ (point) 1)) 10))
	    (progn
	      (forward-line 2)
	      (end-of-line)
	      (innerloop)))
	   ;; otherwise, delete the newline, insert a space, go to next
	   ;; EOL, and loop.
	   (t
	    (progn
	      (delete-char 1)
	      (insert " ")
	      (end-of-line)
	      (innerloop))))))
      (innerloop)

      ;; return point to where it was previously.
      (goto-char p))))
