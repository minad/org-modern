;;; org-modern-indent.el --- org-indent blocks like org-modern -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2023  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/org-modern-indent
;; Package-Requires: ((emacs "27.1") (org "9.5.2") (compat "29.1.4.0"))
;; Version: 0.1.3
;; Keywords: convenience
;; Prefix: org-modern-indent
;; Separator: -

;; org-modern-indent is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; org-modern-indent is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-modern-indent provides the block highlighting of org-modern,
;; when org-indent is enabled.
;;   
;; Can be used with or without org-modern enabled.

;;; Code:
(require 'compat)
(require 'org-indent)
(eval-when-compile (require 'cl-lib))

(defgroup org-modern-indent nil
  "Org-modern style blocks which works with org-indent."
  :group 'org
  :prefix "org-modern-indent-")

;; Face for org-modern-indent line
(defface org-modern-bracket-line '((t (:inherit (org-meta-line) :weight light)))
  "Face for bracket line in org-modern-indent."
  :group 'faces)

(defconst org-modern-indent-begin (propertize "╭" 'face 'org-modern-bracket-line))
(defconst org-modern-indent-guide (propertize "│" 'face 'org-modern-bracket-line))
(defconst org-modern-indent-end   (propertize "╰" 'face 'org-modern-bracket-line))

(defvar org-modern-indent-begin-re
  "\\([ \t]*\\)\\(#\\+\\)\\(?:begin\\|BEGIN\\)_\\S-")
(defvar org-modern-indent--font-lock-keywords
  `((,(concat "^" org-modern-indent-begin-re)
     (0 (org-modern-indent--block-bracket)))))

(defun org-modern-indent--block-bracket ()
  "Prettify blocks with in-text brackets.
For use with `org-indent'.  Uses either in-text brackets, for
auto-indented org text (with real spaces in the buffer, e.g. in
plain lists), or `line-prefix' brackets, when the #+begin part of
the block is flush left in the buffer."
  (save-excursion
    (goto-char (match-beginning 0))
    (if (eq (length (match-string 1)) 0)
	(org-modern-indent--block-bracket-flush)
      (org-modern-indent--block-bracket-indented))))

(defvar org-modern-indent--block-prefixes (make-hash-table :test 'eq))
(defun org-modern-indent--block-bracket-prefix (prefix)
  "Return a vector of 3 prefix strings based on the length of the current PREFIX.
The three returned prefixes include begin, end, and guide bracket
indicators, and are cached by prefix length, for speed.
Additionally, the original prefix string is included at the end
of the returned vector.  If PREFIX is nil or empty, nil is returned."
  (unless (or (not prefix) (string-empty-p prefix))
    (let* ((l (length prefix)))
      (or (gethash l org-modern-indent--block-prefixes)
	  (puthash l (cl-loop for type in '("begin" "guide" "end")
			      for tstr = (symbol-value
					  (intern (concat "org-modern-indent-" type)))
			      with pstr = (substring prefix 0 -1)
			      collect (concat pstr tstr) into prefix-brackets
			      finally return (vconcat prefix-brackets (list prefix)))
		   org-modern-indent--block-prefixes)))))

(defun org-modern-indent--block-bracket-flush ()
  "Insert brackets for org blocks flush with the line prefix."
  (let* ((lpf (get-text-property (point) 'line-prefix))
	 (beg (match-beginning 0))
	 (pind (match-beginning 2))
	 (vec (org-modern-indent--block-bracket-prefix lpf))
	 (block-start (min (line-end-position) (point-max))))
    (with-silent-modifications
      (put-text-property pind (1+ pind) 'org-modern-indent-block-type 'flush)
      (when vec
	(add-text-properties beg block-start
			     `( line-prefix ,(aref vec 0)
				wrap-prefix ,(aref vec 1)))
	(goto-char (match-end 0))
	(when (re-search-forward "^[ \t]*#\\+\\(?:end\\|END\\)_" nil 'noerror)
	  (let ((b (line-beginning-position))
		(p (line-beginning-position 2)))
	    (add-text-properties (1+ block-start) p
				 `(line-prefix ,(aref vec 1) wrap-prefix ,(aref vec 1)))
	    (add-text-properties b (min (line-end-position) (point-max))
				 `(line-prefix ,(aref vec 2) wrap-prefix ,(aref vec 2)))))))))

(defun org-modern-indent--block-bracket-indented ()
  "Insert brackets on space-indented org blocks, e.g. within plain lists."
  (let* ((pf (get-text-property (point) 'line-prefix)) ; prefix from org-indent
	 (pind (match-beginning 2))		       ; at the #
	 (flush (eq (get-text-property pind 'org-modern-indent-block-type) 'flush))
	 (indent (current-indentation)) ; space up to #+begin_
	 (block-indent (+ (point) indent))
	 (search (concat "^[[:blank:]]\\{" (number-to-string indent) "\\}"))
	 (wrap (concat (make-string (if pf (+ indent (length pf) -1) indent) ?\s)
		       org-modern-indent-guide))
	 orig-prefix)
    (with-silent-modifications
      (when flush		  ; formerly this block was flush left
	(when-let ((vec (org-modern-indent--block-bracket-prefix pf)))
	  (setq pf (aref vec 3)	       ; for resetting prefix to saved
		orig-prefix `(line-prefix ,pf))
	  (add-text-properties (point) (min (line-beginning-position 2) (point-max))
			       `(line-prefix ,pf wrap-prefix ,pf))) ; restore
	(put-text-property pind (1+ pind) 'org-modern-indent-block-type 'indent))
     
      (put-text-property (point) block-indent 'face nil)
      (put-text-property (1- block-indent) block-indent
			 'display org-modern-indent-begin)
      (while
	  (progn
	    (add-text-properties
             (point) (min (line-beginning-position 2) (point-max))
             `(wrap-prefix ,wrap ,@orig-prefix))
	    (forward-line)
	    (setq block-indent (+ (point) indent))
	    (let ((lep (line-beginning-position 2)))
	      (when (< block-indent lep)
		(put-text-property (point) block-indent 'face nil))
	      (cond
	       ((eobp) nil)
	       ((looking-at "^\\([ \t]*\\)#\\+\\(?:end\\|END\\)_")
		(if (>= (length (match-string 1)) indent)
		    (put-text-property (1- block-indent) block-indent
				       'display org-modern-indent-end))
		(when flush
		  (add-text-properties
		   (point) (min (line-beginning-position 2) (point-max))
		   `(wrap-prefix ,pf ,@orig-prefix)))
		nil)
	       (t (if (and (<= block-indent lep) (looking-at-p search))
		      (put-text-property (1- block-indent) block-indent
					 'display org-modern-indent-guide))
		  t))))))))

(defun org-modern-indent--wait-and-refresh (buf)
  "Wait for org-indent to finish initializing BUF, then refresh."
  (if (or (not (bound-and-true-p org-indent-agentized-buffers))
	  (not (memq buf org-indent-agentized-buffers)))
      (progn
	(when (buffer-live-p buf) 	; org-capture buffers vanish fast
	  (with-current-buffer buf
	    (font-lock-add-keywords nil org-modern-indent--font-lock-keywords t)
	    (font-lock-flush))))
    (run-at-time 0.1 nil #'org-modern-indent--wait-and-refresh buf)))

(defun org-modern-indent--refresh ()
  "Unfontify entire buffer and refresh line prefix."
  (with-silent-modifications
    (if org-indent-mode
	(org-indent-refresh-maybe (point-min) (point-max) nil))
    (font-lock-flush)))

(defun org-modern-indent--unfontify-wrap (fun)
  "Wrap FUN with extra font lock keywords."
  (lambda (beg end)
    (let* ((font-lock-extra-managed-props
	    (append '(display invisible) font-lock-extra-managed-props)))
      (funcall fun beg end))))

(defvar org-modern-indent--refresh-args nil)
(defun org-modern-indent--store-refresh-args (args)
  "Store the arguments ARGS for `org-indent-add-properties'.
To be added as :filter-args advice."
  (setq org-modern-indent--refresh-args args))

(defun org-modern-indent--refresh-watch (fun beg end &rest r)
  "Watch for org-indent heading refreshes and rebuild prefixes as needed.
FUN is the wrapped function `org-indent-refresh-maybe', and BEG,
END, and R are its arguments."
  (let ((hmod org-indent-modified-headline-flag) p end2 is-flush)
    (apply fun beg end r)
    ;; Recover the args just passed to org-indent-add-properties
    (setq end2 (cadr org-modern-indent--refresh-args))
    (when (or hmod (/= end end2))
      (setq p (1- beg))
      (save-excursion
	(while (< (setq p (next-single-property-change
			   (1+ p) 'org-modern-indent-block-type nil end2))
		  end2)
	  (goto-char p)
	  (setq is-flush
		(eq (get-text-property p 'org-modern-indent-block-type) 'flush))
	  (when (looking-at org-modern-indent-begin-re)
	    (if is-flush
		(org-modern-indent--block-bracket-flush)
	      (org-modern-indent--block-bracket-indented))))))))

(defvar org-modern-indent--initted nil)
(defvar org-modern-indent--old-unfontify-function nil)
;;;###autoload
(define-minor-mode org-modern-indent-mode
  "Org-modern-like block brackets within org-indent."
  :global nil
  :group 'org-modern-indent
  
  (if org-modern-indent-mode
      (progn
	(advice-add 'org-indent-refresh-maybe :around
		    #'org-modern-indent--refresh-watch)
	(advice-add 'org-indent-add-properties :filter-args
		    #'org-modern-indent--store-refresh-args)
	(org-modern-indent--wait-and-refresh (current-buffer)))
    ;; Disabling
    (advice-remove 'org-indent-refresh-maybe
		   #'org-modern-indent--refresh-watch)
    (advice-remove 'org-indent-add-properties
		   #'org-modern-indent--store-refresh-args)
    (font-lock-remove-keywords nil org-modern-indent--font-lock-keywords)
    (org-modern-indent--refresh)))

(provide 'org-modern-indent)
;;; org-modern-indent.el ends here
