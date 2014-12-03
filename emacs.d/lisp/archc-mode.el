;;; archc-mode-el -- Major mode for editing ArchC files

;; Author: Sandro Rigo <srigo@ic.unicamp.br>
;; Created: 19 05 2003
;; Keywords: ArchC major-mode

;; Copyright (C) 2003 Sandro Rigo <srigo@ic.unicamp.br>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;; 
;; This mode is based on an example created by Scott Andrew Borton
;; and used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar archc-mode-hook nil)
(defvar archc-mode-map nil
  "Keymap for ArchC major mode.")

(if archc-mode-map nil
  (setq archc-mode-map (make-keymap)))

(setq auto-mode-alist
	  (append
	   '(("\\.ac\\'" . archc-mode))
	   auto-mode-alist))

(defconst archc-font-lock-keywords-1
  (list
   ; These are the begining of sections in ArchC descriptions
   '("\\<AC_\\(ISA\\|ARCH\\)\\>" . font-lock-function-name-face )
   '("\\<ARCH_CTOR\\>" . font-lock-function-name-face )
   '("\\<ISA_CTOR\\>" . font-lock-function-name-face )

   ;These are keywords into AC_ARCH and AC_ISA descriptions
   '("\\<ac_\\(format\\|instr\\|wordsize\\|mem\\|cache\\|reg\\|regbank\\|stage\\)\\>" . font-lock-keyword-face )
   '("\\<ac_i\\(mem\\|cache\\)\\>" . font-lock-keyword-face )
   '("\\<ac_d\\(mem\\|cache\\)\\>" . font-lock-keyword-face )
   '("ac_isa\\|set_decoder\\|set_asm\\|set_cycles\\|set_endian\\|bindsTo" . font-lock-builtin-face)
   '("<.*>" . font-lock-type-face )
   '(":[0-9]+" . font-lock-constant-face )
   '("\\<[0-9]+\\>" . font-lock-constant-face )
   '("=\ *0x[0-9a-zA-Z]+" . font-lock-constant-face ))
  "Syntax-Highlighting in ArchC files.")

(defvar archc-font-lock-keywords archc-font-lock-keywords-1
  "Default highlighting expressions for pattlib mode.")

(defun archc-indent-line ()
  "Indent current line as ArchC code."
  (setq default-tab-width 2)
  (interactive)
  (beginning-of-line)
  (if (looking-at "[:blank:]*#.*")
      (ident-line-to 0) )
  (if (bobp)
      (indent-line-to 0)		   ; First line is always non-indented
     (let ((not-indented t) cur-indent)
      (if (looking-at ".*}.*") ; If the line we are looking at is the end of a block, then decrease the indentation
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at ".*}.*") ; This hint indicates that we need to indent at the level of the END_ token
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at ".*{.*") ; This hint indicates that we need to indent an extra level
		  (progn
		    (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation


(defvar archc-mode-syntax-table nil
  "Syntax table for archc-mode.")

(defun archc-create-syntax-table ()
  (if archc-mode-syntax-table
	  ()
	(setq archc-mode-syntax-table (make-syntax-table))
	(set-syntax-table archc-mode-syntax-table)
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" archc-mode-syntax-table)
  
	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" archc-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" archc-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" archc-mode-syntax-table)))

(defun archc-mode ()
  "Major mode for editing archc patterns files."
  (interactive)
  (kill-all-local-variables)
  (archc-create-syntax-table)
  
  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
		'(archc-font-lock-keywords))
  
  ;; Register our indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'archc-indent-line)
  
  (setq major-mode 'archc-mode)
  (setq mode-name "ArchC")
  (run-hooks 'archc-mode-hook))

(provide 'archc-mode)

;;; archc-mode.el ends here
