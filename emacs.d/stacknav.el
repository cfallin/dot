;;; stacknav.el: a little stack-based navigation hack.
;;;
;;; Copyright (c) 2022 Chris Fallin <chris@cfallin.org>.
;;;
;;; Released under the MIT License.

(defvar stacknav-line-preview-length 40 "Length of line previews in stacknav locations.")

(defvar stacknav--back nil "Back-list for stacknav.")
(defvar stacknav--fwd nil "Foward-list for stacknav.")
(defvar stacknav--cleared nil "List of cleared locs for stacknav.")

(defun stacknav-mark ()
  "Mark current point as a discrete point in stacknav's history."
  (interactive)
  (let ((l (stacknav--get-loc)))
    (setq stacknav--back
          (stacknav--push-and-remove-dups l stacknav--back))))

(defun stacknav-go-back ()
  "Go back one mark."
  (interactive)
  (if (not stacknav--back)
      (error "stacknav-go-back: at beginning of history!"))
  (let ((l (pop stacknav--back)))
    (setq stacknav--fwd
          (stacknav--push-and-remove-dups (stacknav--get-loc) stacknav--fwd))
    (stacknav--goto-loc l)))

(defun stacknav-go-fwd ()
  "Go forward one mark."
  (interactive)
  (if (not stacknav--fwd)
      (error "stacknav-go-fwd: at end of history!"))
  (let ((l (pop stacknav--fwd)))
    (setq stacknav--back
          (stacknav--push-and-remove-dups (stacknav--get-loc) stacknav--back))
    (stacknav--goto-loc l)))

(defun stacknav-clear-fwd ()
  "Clear forward list."
  (interactive)
  (nconc stacknav--cleared stacknav--fwd)
  (setq stacknav--fwd nil))

(defun stacknav-clear-back ()
  "Clear backward list."
  (interactive)
  (nconc stacknav--cleared stacknav--back)
  (setq stacknav--back nil))

(defun stacknav-clear-cleared ()
  "Clear the cleared list (i.e., empty the trash)."
  (interactive)
  (setq stacknav--cleared nil))

(defun stacknav-pick ()
  "Pick one mark in navigation history (backward or forward)."
  (interactive)
  (let ((action (lambda (loc)
                  (stacknav-mark)
                  (stacknav--goto-loc loc)))
        (create-candidates (lambda (l)
                             (mapcar (lambda (tuple) (cons (nth 2 tuple) tuple)) l))))
    (let ((sources `(((name . "Stacknav Back")
                      (candidates . ,(funcall create-candidates stacknav--back))
                      (action . (("Goto Point" . ,action))))
                     ((name . "Stacknav Forward")
                      (candidates . ,(funcall create-candidates stacknav--fwd))
                      (action . (("Goto Point" . ,action))))
                     ((name . "Stacknav Cleared Locs")
                      (candidates . ,(funcall create-candidates stacknav--cleared))
                      (action . (("Goto Point" . ,action)))))))      
      (helm :sources sources))))

(defun stacknav--loc-eq (l1 l2)
  (and
   (eq (nth 0 l1) (nth 0 l2))
   (eq (nth 1 l1) (nth 1 l2))))

(defun stacknav--push-and-remove-dups (loc stack)
  (let ((filtered (mapcan (lambda (existing-loc)
                            (if (not (stacknav--loc-eq loc existing-loc))
                                (list existing-loc)
                              (list)))
                          stack)))
    (push loc filtered)
    filtered))

(defun stacknav--get-loc ()
  "Get current location."
  (let* ((b (current-buffer))
         (pos (point))
         (line-number (line-number-at-pos pos))
         (headerline (if (and
                          (boundp 'lsp-headerline--string)
                          lsp-headerline--string)
                         (format " [ %s ]" lsp-headerline--string)
                       ""))
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (line-trimmed (string-trim line))
         (line-preview (if (> (length line-trimmed) stacknav-line-preview-length)
                           (concat (substring line-trimmed 0 stacknav-line-preview-length) " [ ... ]")
                         line-trimmed)))
    (list b pos (format "%s line %d%s: %s" b line-number headerline line-preview))))

(defun stacknav--goto-loc (tuple)
  "Go to a given location, as returned by stacknav--get-loc."
  (let ((b (nth 0 tuple))
        (pos (nth 1 tuple)))
    (if (not (eq (current-buffer) b))
        (switch-to-buffer b))
    (goto-char pos)))

;; Hooks

(defvar stacknav--saved-loc)

(defun stacknav--begin-hook ()
  (setq stacknav--saved-loc (stacknav--get-loc)))

(defun stacknav--end-hook ()
  (let ((cur-loc (stacknav--get-loc)))
    (if (and stacknav--saved-loc
             (not (stacknav--loc-eq cur-loc stacknav--saved-loc)))
        (setq stacknav--back
              (stacknav--push-and-remove-dups stacknav--saved-loc stacknav--back))))
  (setq stacknav--saved-loc nil))

(add-hook 'isearch-mode-hook 'stacknav--begin-hook)
(add-hook 'isearch-mode-end-hook 'stacknav--end-hook)
(add-hook 'helm-before-initialize-hook 'stacknav--begin-hook)
(add-hook 'helm-after-action-hook 'stacknav--end-hook)

;; Keyboard bindings
(defvar stacknav-keymap
  (let ((k (make-sparse-keymap)))
    (define-key k "\M-m" 'stacknav-mark)
    (define-key k "\M-d" 'stacknav-mark)
    (define-key k "\M-p" 'stacknav-pick)
    (define-key k "\M-s" 'stacknav-pick)
    (define-key k "\M-f" 'stacknav-go-fwd)
    (define-key k "\M-b" 'stacknav-go-back)
    (define-key k "cf" 'stacknav-clear-fwd)
    (define-key k "cb" 'stacknav-clear-back)
    (define-key k "cc" 'stacknav-clear-cleared)
    k))

(define-key global-map "\M-s" stacknav-keymap)
