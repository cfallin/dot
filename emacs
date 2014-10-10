(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode 1)
 '(explicit-shell-file-name "/bin/bash")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(standard-indent 2))

(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 4)

(if (fboundp 'iswitchb-mode)
 (iswitchb-mode 1))
;;(if (fboundp 'cua-mode)
;; (cua-mode 1))

(add-to-list 'load-path "~/.emacs.d")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'verilog-mode "verilog-mode" "Major mode for editing Verilog." t)

(setq auto-mode-alist
      (append
       '(("\\.h$" . c++-mode))
       '(("\\.cs$" . csharp-mode))
       '(("\\.v$" . verilog-mode))
       '(("\\.vh$" . verilog-mode))
       auto-mode-alist))

(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   1
      verilog-case-indent              2
      verilog-auto-newline             nil
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-auto-endcomments         nil
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              nil
      verilog-highlight-p1800-keywords nil)


(global-set-key [ (meta s) ] 'save-buffer)
(global-set-key [ (meta o) ] 'find-file-other-frame)
;(global-set-key [ (meta q) ] 'save-buffers-kill-emacs) ; consistent with OS X
;(global-set-key [ (control q) ] 'fill-paragraph) ; remapped from default M-q

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

; Org-Mode (http://orgmode.org/manual/Activation.html#Activation)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files '("~/Documents/orgmode/work.org" "~/Documents/orgmode/personal.org"))
(setq org-log-done 'time)

(global-set-key [f1] 'iswitchb-buffer)
(global-set-key [f2] 'other-window)
(global-set-key [f3] 'delete-other-windows)
(global-set-key [f4] 'kill-this-buffer)

(defun word-count nil "Count words in current region"  (interactive)
  (shell-command-on-region (mark) (point) "wc"))

(global-set-key [f5] 'word-count)

;(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(defun strip-string (s)
  (let* ((l (length s))
         (last-char (if (> l 0)
                        (substring s -1 nil)
                      "")))
    (if (string= last-char "\n")
        (substring s 0 -1)
      s)))

(defun my-hostname ()
  (strip-string (shell-command-to-string "hostname")))


;(add-to-list 'load-path "~/.emacs.d/evil")
;(require 'evil)
;(if nil
;    (evil-mode 1)
;    ; map C-u to page-up, like in Vim, rather than the Emacs command prefix
;    (define-key evil-normal-state-map "\C-u" 'evil-scroll-up))

(if (fboundp 'color-theme-gnome2)
    (color-theme-gnome2))

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

(global-set-key "\C-cm" 'un-microsoft-ify)

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
          ;; if at end of paragraph, skip to the next para and loop.
          (when (< (point) (- end 2))
            (when (and (= (char-after (point)) 10)
                       (= (char-after (+ (point) 1)) 10))
              (forward-line 2)
              (end-of-line)
              (innerloop)))
          ;; otherwise, delete the newline, insert a space, go to next
          ;; EOL, and loop.
          (delete-char 1)
          (insert " ")
          (end-of-line)
          (innerloop)))
      (innerloop)

      ;; return point to where it was previously.
      (goto-char p))))

(global-set-key "\C-cu" 'unwrap)

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

(add-to-list 'load-path "~/.emacs.d/epl")
(require 'package)
(add-to-list 'load-path "~/.emacs.d/dash.el")
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/pkg-info.el")
(require 'pkg-info)
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)
(add-to-list 'load-path "~/.emacs.d/cider")
(require 'cider)

(menu-bar-mode 0)
(tool-bar-mode 0)
