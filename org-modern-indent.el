;;; org-modern-indent.el --- Indent blocks like org-modern -*- lexical-binding: t -*-
;; Copyright (C) 2022-2025  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/org-modern-indent
;; Package-Requires: ((emacs "27.1") (org "9.6.1") (compat "30.0"))
;; Version: 0.5.1
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

;; org-modern-indent approximates the block highlighting style of
;; org-modern, when org-indent is enabled.
;;   
;; Can be used with or without org-modern.

;;; Code:
(require 'compat)
(require 'org-indent)
(require 'org-element)
(eval-when-compile (require 'cl-lib))

;;;; Face and style
(defgroup org-modern-indent nil
  "Org-modern style blocks which works with org-indent."
  :group 'org
  :prefix "org-modern-indent-")

;; Face for org-modern-indent line
(defface omi/bracket-line
  '((t (:inherit (org-meta-line) :weight light)))
  "Face for bracket line in org-modern-indent."
  :group 'faces)

(defconst omi/begin (propertize "╭" 'face 'omi/bracket-line))
(defconst omi/guide (propertize "│" 'face 'omi/bracket-line))
(defconst omi/end   (propertize "╰" 'face 'omi/bracket-line))

(defvar omi/begin-re
  "\\([ \t]*\\)\\(#\\+\\)\\(?:begin\\|BEGIN\\)_\\S-")

;;;; Utility
(defsubst omi/-lbp (p &optional n)
  "Return the line beginning position N at point P."
  (save-excursion (goto-char p) (pos-bol n)))

;;;; Block prefix cache
(defvar omi/-prefixes (make-hash-table :test #'equal)
  "A hash table of cached line prefixes.
The hash index is a combination of the (unstyled) `org-indent' length
and the \"real\" (space) indentation at the block beginning.  Each hash
value is a vector of length 3:

  [BEGIN GUIDE END]

holding the wrap/indent prefix strings for the block begin, end, and
guide (body).")

(defun omi/-prefix (org-indent extra-indent)
  "Cache and return prefix strings based on the indent levels.
There are 2 types of indentation: ORG-INDENT (styled with `org-indent'
face), and EXTRA-INDENT (unstyled).  See `org-modern-indent--prefix' for
the contents."
  (unless (and (= org-indent 0) (= extra-indent 0))
    (let* ((key (+ org-indent (* 1000 extra-indent)))
	   (cached (gethash key omi/-prefixes)))
      (or cached
	  (puthash key
		   (cl-loop for type in '("begin" "guide" "end")
			    for tstr = (symbol-value
					(intern (concat "org-modern-indent-" type)))
			    with olen = (if (> extra-indent 0) org-indent
					  (1- org-indent))
			    with ostr = (and (> olen 0)
					     (propertize (make-string olen ?\s)
							 'face '(org-indent default)))
			    with pstr = (and (> extra-indent 0)
					     (make-string (1- extra-indent) ?\s))
			    collect (concat ostr pstr tstr))
		   omi/-prefixes)))))

;;;; Finding and operating on blocks
(defsubst omi/-block-p (el)
  "Return non-nil if EL is an org block element."
  (when-let ((el) (type (car el)))
    (memq type
	  '(center-block comment-block dynamic-block
			 example-block export-block quote-block
			 special-block src-block verse-block))))

(defun omi/-block-at-point (&optional pos)
  "Return the org block element at POS, if any.
Considers both the element at point, and its parent.  Does not consider
affiliated content like title as part of the block."
  (let ((pos (or pos (point))))
    (when-let ((element (org-element-at-point pos)))
      (when (or (omi/-block-p element)
		(omi/-block-p
		 (setq element (org-element-property :parent element))))
	(unless (< pos (org-element-property :post-affiliated element))
	  element)))))

(defsubst omi/-block-beg-end (node)
  "Return a cons of NODE's (BEG . END).
BEG is at the beginning of the #+BEGIN line, and END is at the end of
the #+END line."
  (let* ((bbeg (org-element-property :post-affiliated node)) ; on #+begin
	 (bend (save-excursion
		 (goto-char (org-element-property :end node))
		 (skip-chars-backward "\n\t ")
		 (point))))
    (cons bbeg bend)))

(defun omi/-walk-blocks (beg end fun)
  "Walk all org blocks intersecting the region from BEG..END.
FUN is called on each block with the beginning and end positions of the
block (at the beginning and end of the lines containing the header and
footer, respectively).  Note that the first block's beginning may lie
prior to BEG, and the last block's end my fall past END, if either only
partially overlaps the region."
  (org-with-wide-buffer
   (goto-char beg)
   (let ((node (omi/-block-at-point)) finished)
     (while (not finished)
       (when node
	 (cl-destructuring-bind (bbeg . bend) (omi/-block-beg-end node)
	   (funcall fun bbeg bend)
	   (goto-char bend)
	   (when (>= bend end) (setq finished t)))) ; last block
       (unless finished
	 (if (re-search-forward omi/begin-re end t)
	     (setq node (omi/-block-at-point))
	   (setq finished t)))))))

;;;; Drawing block brackets
(defun omi/-draw-block (beg end beg0 end0 org-indent real-indent)
  "Insert brackets for block between BEG and END.
BEG0 and END0 represents the block's full range, corresponding to the
line beginning/end positions of the block's #+BEGIN/END header/footer
lines.  The `org-indent' prefix length at block header is ORG-INDENT,
and REAL-INDENT is the amount of \"real\" (hard space) block
indentation.  REAL-INDENT may be zero."
  (with-silent-modifications
    (pcase-let* (((seq beg-prefix guide-prefix end-prefix)
		  (omi/-prefix org-indent real-indent))
		 ((seq beg-display guide-display end-display)
		  (and (> real-indent 0) (omi/-prefix real-indent 0)))
		 (beg-bol (omi/-lbp beg))
		 (body-start (max (omi/-lbp beg0 2) beg-bol)))
      (cl-macrolet ((add-prefixes (pbeg pend line-prefix wrap-prefix)
		      `(add-text-properties ,pbeg ,pend
			`(,@(and (= real-indent 0) `(line-prefix ,,line-prefix))
			  wrap-prefix ,,wrap-prefix)))
		    (add-guides (pbeg pend display-str)
		      `(add-text-properties ,pbeg ,pend
			`( omi/display ,,display-str rear-nonsticky t))))
	(when (> body-start beg)  ;; BEGIN
	  (add-prefixes beg-bol body-start beg-prefix guide-prefix)
	  (when (> real-indent 0)
	    (add-guides beg-bol (+ beg-bol real-indent) beg-display)))
	(when (> end body-start)  ;; GUIDE BODY
	  (let* ((end-bol (omi/-lbp end))
		 (end0-bol (omi/-lbp end0))
		 (after-end (omi/-lbp end 2))
		 (body-final (if (= end-bol end0-bol) end-bol after-end)))
	    (add-prefixes body-start body-final guide-prefix guide-prefix)
	    (when (> real-indent 0)
	      (goto-char body-start)
	      (while (< (point) body-final)
		(when (>= (current-indentation) real-indent)
		  (add-guides (point) (+ (point) real-indent) guide-display))
		(forward-line)))
	    (when (= end-bol end0-bol) ;; END
	      (if (= real-indent 0)
		  (add-prefixes end-bol after-end end-prefix end-prefix)
		(goto-char end-bol)
		(when (>= (current-indentation) real-indent)
		  (add-guides end-bol (+ end-bol real-indent) end-display))))))))))

;;;; org-indent interfacing
(defvar-local omi/-level-change-end nil)
(defun omi/-refresh-maybe-watch (fun beg end &rest r)
  "Watch for org-indent heading refreshes and record the new end position.
FUN is the wrapped function `org-indent-refresh-maybe', and BEG,
END, and R are its arguments."
  ;; sadly org-indent-refresh-maybe modifies beg/end but does not return
  (let ((oiap-orig (symbol-function 'org-indent-add-properties)))
    (cl-letf (((symbol-function 'org-indent-add-properties)
	       (lambda (b e &optional delay)
		 (setq omi/-level-change-end (and (> e end) e))
		 (funcall oiap-orig b e delay))))
      (apply fun beg end r))))

(defun omi/-strip-display (beg end)
  (with-silent-modifications
    (remove-text-properties beg end '(omi/display nil))))

(defvar-local omi/-init nil)
(defun omi/-wait-and-refresh (buf)
  "Wait for org-indent to finish initializing BUF, then refresh."
  (if (or (not (bound-and-true-p org-indent-agentized-buffers))
	  (not (memq buf org-indent-agentized-buffers)))
      (omi/init buf)
    ;; still waiting
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(if omi/-init
	    (let ((cnt (cl-incf (cadr omi/-init))))
	      (if (> cnt 5)
		  (user-error
		   "org-modern-indent: Gave up waiting for %s to initialize"
		   buf)
		(timer-activate
		 (timer-set-time (car omi/-init)
				 (time-add (current-time) 0.2)))))
	  (setq omi/-init
		(list (run-at-time 0.1 nil #'omi/-wait-and-refresh buf)
		      1)))))))

(defun omi/-refresh ()
  "Strip custom display property and refresh line prefix in entire buffer."
  (with-silent-modifications
    (org-with-wide-buffer
     (when org-indent-mode
       (omi/-strip-display (point-min) (point-max))
       (org-indent-refresh-maybe (point-min) (point-max) nil)))))

;;;; Before/after change
(defun omi/-intersects-leader-at (target beg end)
  "Return non-nil if BEG..END intersects initial non-blank text at TARGET."
  (save-excursion
    (goto-char target)
    (back-to-indentation)
    (let ((beg-targ (point))
	  (end-targ (progn (skip-chars-forward "^[:blank:]\n") (point))))
      (and (<= beg end-targ) (<= beg-targ end)))))

(defun omi/-before-change (beg end)
  "Check the BEG..END range for modified block header/footer lines.
Adds an `:org-modern-indent-block' text property to the block, with
value `damaged' for damaged block."
  (cl-labels
      ((detect-block-damage (bl-beg bl-end)
	 "Detect and mark damaged blocks."
	 (unless (and (<= beg bl-beg) (>= end bl-end)) ; entire block replaced/deleted
	   (with-silent-modifications
	     (if (or (omi/-intersects-leader-at bl-beg beg end)
		     (omi/-intersects-leader-at bl-end beg end))
		 (progn
		   (put-text-property bl-beg (1+ bl-beg) :omi/indent nil) ; now unknown!
		   (put-text-property bl-beg (omi/-lbp bl-end 2) :omi/block 'damaged))
	       (put-text-property bl-beg (omi/-lbp bl-end 2) :omi/block t))))))
    (omi/-walk-blocks beg end #'detect-block-damage)))

(defun omi/-pos-in-leader-p (pos)
  "Return non-nil if a block's header/footer leader text encompasses POS."
  (when-let ((node (omi/-block-at-point pos)))
    (org-with-wide-buffer
     (cl-destructuring-bind (bbeg . bend) (omi/-block-beg-end node)
       (or (omi/-intersects-leader-at bbeg pos pos)
	   (omi/-intersects-leader-at bend pos pos))))))

(defun omi/-after-change (beg end _len)
  "Perform after-change operations on changed text from BEG..END.
To be added after `org-indent-refresh-maybe' on
`after-change-functions'."
  (with-silent-modifications
    (let ((beg-block (get-text-property beg :omi/block))
	  (end-block (get-text-property end :omi/block))
	  extend)
      (when (and beg-block (or (eq beg-block 'damaged) (omi/-pos-in-leader-p beg)))
	(setq extend t
	      beg (or (previous-single-property-change beg :omi/block) beg)))
      (when (and end-block (or (eq end-block 'damaged) (omi/-pos-in-leader-p end)))
	(setq extend t
	      end (or (next-single-property-change end :omi/block) end)))
      (when extend
	(remove-text-properties beg end '(:omi/block nil :omi/indent nil))
	(org-indent-add-properties beg end)))
    ;; If we had a level change, extend down to the next heading
    (when omi/-level-change-end (setq end (max end omi/-level-change-end)))
    (setq beg (omi/-lbp beg) end (omi/-lbp end 2))
    (remove-text-properties beg end '(omi/display nil))
    (omi/-walk-blocks beg end
     (lambda (bl-beg bl-end)
       (let ((beg0 bl-beg) (end0 bl-end)
	     (pf (get-text-property bl-beg 'line-prefix))
	     oi-pos oi ci full-block)
	 (save-excursion
	   (goto-char bl-beg)
	   (back-to-indentation)
	   (setq oi-pos (point)
		 oi (get-text-property oi-pos :omi/indent)
		 ci (current-indentation)))
	 (unless (eq oi ci)
	   (put-text-property oi-pos (1+ oi-pos) :omi/indent ci))
	 (when (and oi (= oi 0) (/= oi ci)) ; was flush, is now indented:
	   (org-indent-add-properties bl-beg bl-end)) ; repair block's prefixes
	 (when (< bl-beg beg)		; 1st block straddles BEG
	   (if (eq oi ci) (setq bl-beg beg) (setq full-block t)))
	 (when (> bl-end end)		; last block straddles END
	   (if (eq oi ci) (setq bl-end end) (setq full-block t)))
	 (when full-block
	   (with-silent-modifications
	     (remove-text-properties bl-beg bl-end '(omi/display nil))))
	 (omi/-draw-block bl-beg bl-end beg0 end0 (length pf) ci))))))

;;;; Mode/setup
(defun omi/init (&optional buf)
  "Register buffer BUF and refresh.
To be added to `org-indent-post-buffer-init-functions'."
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (when (buffer-live-p buf)	     ; org-capture buffers vanish fast
      (with-current-buffer buf
	(add-hook 'before-change-functions #'omi/-before-change nil t)
	(or (memq #'omi/-after-change after-change-functions)
	    (cl-loop for func on after-change-functions
		     if (eq (car func) 'org-indent-refresh-maybe) do
		     (setcdr func (cons #'omi/-after-change (cdr func))) and return t)
	    (add-hook 'after-change-functions #'omi/-after-change 98 t))
	(org-with-wide-buffer (omi/-after-change (point-min) (point-max) nil))
	(setq omi/-init t)))))

;;;###autoload
(define-minor-mode omi/mode
  "Org-modern-like block brackets within org-indent."
  :global nil
  :group 'org-modern-indent
  (if omi/mode
      (progn
	(advice-add 'org-indent-refresh-maybe :around
		    #'omi/-refresh-maybe-watch)
	(cond
	 ;; already registered before, just toggle
	 ((or (called-interactively-p 'any) omi/-init) (omi/init))
	 ;; Register with buffer init
	 ((boundp 'org-indent-post-buffer-init-functions)
	  (add-hook 'org-indent-post-buffer-init-functions #'omi/init nil t))
	 ;; No hook available, use the less reliable method
	 (t (omi/-wait-and-refresh (current-buffer))))
	(cl-pushnew 'omi/display
		    (alist-get 'display char-property-alias-alist)))
    ;; Disabling
    (advice-remove 'org-indent-refresh-maybe #'omi/-refresh-maybe-watch)
    (remove-hook 'before-change-functions #'omi/-before-change t)
    (remove-hook 'after-change-functions #'omi/-after-change t)
    (if (boundp 'org-indent-post-buffer-init-functions)
	(remove-hook 'org-indent-post-buffer-init-functions #'omi/init t)
      (cancel-timer (car omi/-init))
      (setq omi/-init nil))
    (omi/-refresh)))

(provide 'org-modern-indent)
;;; org-modern-indent.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("omi/" . "org-modern-indent-") (":omi/" . ":org-modern-indent-"))
;; package-lint--sane-prefixes: "^omi/"
;; End:
 
