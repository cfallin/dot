;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq column-number-mode t)  ; show column numbers
(setq explicit-shell-filename "/bin/bash")  ; use bash for shell buffers
(setq inhibit-startup-screen t)  ; skip startup screen
(menu-bar-mode 0)  ; no menubar
(tool-bar-mode 0)  ; no toolbar
;;(ido-mode t)  ; Ido mode: smarter C-x b (buffer switching)

(load "~/.emacs.d/themes/meacupla-theme.el")

;; C-x b to a buffer should always show the buffer in the current window,
;; even if open somewhere else -- sometimes we want two windows on the
;; buffer.
(setq display-buffer-overriding-action '(display-buffer-same-window . nil))

;; framemove: windmove motion past the end of a frame should move to
;; the next frame (so e.g. C-c C-l / C-c C-h move right and left
;; across vertically-split windows in a pair of frames on dual
;; monitors).
(require 'cl)
(load "~/.emacs.d/framemove.el")
(setq framemove-hook-into-windmove t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f1] 'switch-to-buffer)
(global-set-key [f2] 'other-window)
(global-set-key [f3] 'delete-other-windows)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f5] 'word-count)
(global-set-key "\C-cm" 'un-microsoft-ify)
(global-set-key "\C-cu" 'unwrap)
(global-set-key "\C-c\C-h" 'windmove-left)
(global-set-key "\C-c\C-l" 'windmove-right)
(global-set-key "\C-c\C-k" 'windmove-up)
(global-set-key "\C-c\C-j" 'windmove-down)
(global-set-key "\C-xo" 'switch-window)
(global-set-key "\C-\\" 'redo) ; C-/ is undo, so C-\ is redo
(global-set-key "\C-ca" 'org-agenda)

;; easier bindings for left and right window movement.
(global-set-key (kbd "C-.") 'windmove-right)
(global-set-key (kbd "C-,") 'windmove-left)

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
	; Rust...
	rust-mode
        cargo
        racer
	company-racer
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
	redo+
	; Window movement helper:
	switch-window))
	

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'notmuch nil 'noerror)
(require 'redo+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
	("\\.svg\\'" "inkscape" (file))))
(openwith-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Org-Mode (http://orgmode.org/manual/Activation.html#Activation)
(setq org-agenda-files '("~/Documents/orgmode/work.org"
			 "~/Documents/orgmode/personal.org"
			 "~/Dropbox/Documents/notes/research.org"))
(setq org-file-apps
      '(("\\.pdf\\'" . "evince %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C, Python and indenting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq standard-indent 2)
(setq py-indent-offset 4)
(setq c-basic-offset 4)
(setq c-default-style "bsd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)
(require 'racer)
(require 'cargo)
(require 'flycheck-rust)

;; N.B.: requires `racer` binary (do `cargo install racer` and adjust
;; `racer-cmd` below as necessary).
;(setq rust-enable-racer t)
(setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")
(setq racer-cmd "~/.cargo/bin/racer")

(add-hook 'rust-mode-hook (lambda ()
			    (racer-activate)
			    (racer-turn-on-eldoc)
			    ;(flycheck-mode)
			    (company-mode)
			    (set (make-local-variable 'company-backends) '(company-racer))
			    (local-set-key (kbd "M-.") #'racer-find-definition)
			    (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
			    (local-set-key (kbd "\C-c\C-t") #'cargo-process-test)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clang-format)
(require 'ycmd)
(require 'company-ycmd)

(company-ycmd-setup)

;; N.B.: requires `ycmd` from https://github.com/Valloric/ycmd.
;; After cloning into ~/.emacs.d/ycmd, do:
;; $ git submodule update --init --recursive
;; $ ./build.py --all
(set-variable 'ycmd-server-command
	      (list "python" (expand-file-name "~/.emacs.d/ycmd/ycmd")))
(set-variable 'ycmd-global-config (expand-file-name "~/.ycm_config.py"))

(add-hook 'c++-mode-hook (lambda ()
			   (company-mode)
;			   (ycmd-mode)
			   (flycheck-mode)
			   (local-set-key (kbd "\C-q") #'clang-format-region)
			   (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prolog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datalog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/souffle-mode.el")

(add-to-list 'auto-mode-alist '("\\.dl\\'" . souffle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java / JastAddJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.jrag\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.jadd\\'" . java-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq geiser-active-implementations '(racket))
(setq geiser-guile-binary "/usr/bin/guile2.2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

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

(defun word-count nil "Count words in current region"  (interactive)
  (shell-command-on-region (mark) (point) "wc"))

(defun un-microsoft-ify nil
  (interactive)

  ; save the region and use it for each replace-regexp.
  (let ((start (point))
        (end (mark)))
    (when end

      ; indented bulletted lists become "-"-bulletted plain text trees
      (replace-regexp "^\x2022\x09" "    - " nil start end)
      (replace-regexp "^o\x09" "      - " nil start end)
      (replace-regexp "^\xf0a7\x09" "        - " nil start end)

      ; special characters: quotes, arrows, ellipses, ...
      (replace-regexp "\xf0e0" "-->" nil start end)
      (replace-regexp "\x2026" "..." nil start end)
      (replace-regexp "\x201c" "\"" nil start end)
      (replace-regexp "\x201d" "\"" nil start end)
      (replace-regexp "\x2018" "'" nil start end)
      (replace-regexp "\x2019" "'" nil start end)
      (replace-regexp "\x2013" "--" nil start end)
      (set-mark end))))

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

(defun open-urls nil
  (interactive)

  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (if (= (length selection) 0)
        (message "no text selected!")
        (let ((parts (split-string selection)))
          (message "selection: ")
          (message selection)
          (defun open-one-url (url)
            (message "opening  url: ")
            (message url)
            (start-process "open-url"
                           (get-buffer-create "*open-url-messages*")
                           "/usr/bin/xdg-open"
                           url))
          (defun open-list (l)
            (if l
                (let ((url (car l))
                      (rem (cdr l)))
                  (open-one-url url)
                  (open-list rem))
              nil))
          (open-list parts)))))
