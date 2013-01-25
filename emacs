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
(add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
