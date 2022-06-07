;;; org-modern-indent.el         -*- lexical-binding: t; -*-
;; Copyright (C) 2022  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/org-modern-indent
;; Package-Requires: ((emacs "27.2") (org "9.5.2") (org-modern)
;; Version: 0.0.1
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

;; org-modern-indent enables the block highlighting of org-modern,
;; even when org-indent is enabled.
;; Requires:
;;   - org-modern
;;   - org-indent-mode enabled

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'org-indent)
(require 'org-modern)
(require 'seq)

(defun org-modern-indent--face-in (faces element)
  "Determine if any of FACES are present in ELEMENT.
FACES must be a list.  A face can be 'present' by being named
explicitly, or inherited."
  (cl-loop for face in faces
	   if (cond ((consp element)
		     (or (memq face element)
			 (if-let ((inh (plist-get element :inherit))) 
			     (if (consp inh)
				 (memq face inh) (eq inh face)))))
		    (t (eq element face)))
	   return t))

(defun org-modern-indent--add-props (beg end line extra-pad &optional guide wrap-guide)
  (with-silent-modifications
    (add-text-properties beg end
			 `(line-prefix
			   ,(concat line guide)
			   wrap-prefix
			   ,(concat line (or wrap-guide guide) extra-pad)))))

(defvar org-modern-indent-begin nil)
(defvar org-modern-indent-guide	nil)
(defvar org-modern-indent-end   nil)
(defun org-modern-indent-set-line-properties (level indentation &optional heading)
  "A redefinition of `org-indent-set-line-properties' for org-modern block style.
Treats blocks specially, by extending the line and wrap prefixes
with a box guide unicode character."
  (let ((line (aref (pcase heading
		      (`nil org-indent--text-line-prefixes)
		      (`inlinetask org-indent--inlinetask-line-prefixes)
		      (_ org-indent--heading-line-prefixes))
		    level))
	(extra-pad (if (> indentation 0)
		       (org-add-props
			   (if heading (concat (make-string level ?*) " ")
			     (make-string indentation ?\s))
			   nil 'face 'org-indent)))
	(change-beg (line-beginning-position))
	(change-end (line-beginning-position 2))
	(line-end (line-end-position)))
    (unless (get-text-property (point) 'fontified) ;sometimes we beat font-lock
      (font-lock-ensure change-beg line-end)) ;We rely on fonts to identify blocks
    (or (when-let (((eq heading nil))
		   (face (get-text-property (point) 'face)))
	  (cond
	   ;; Block begin: use begin prefix, use guide for following blank line + wrap
	   ((org-modern-indent--face-in '(org-block-begin-line) face)
	    (org-modern-indent--add-props change-beg line-end line extra-pad
					  org-modern-indent-begin org-modern-indent-guide)
	    ;; possible blank line following
	    (org-modern-indent--add-props line-end change-end line extra-pad
					  org-modern-indent-guide))

	   ;; Block body: use guide prefix
	   ((org-modern-indent--face-in '(org-block org-quote org-verse) face)
	    (org-modern-indent--add-props change-beg change-end line extra-pad
					  org-modern-indent-guide))

	   ;; Block end: use end prefix
	   ((org-modern-indent--face-in '(org-block-end-line) face)
	    (org-modern-indent--add-props change-beg line-end line extra-pad
					  org-modern-indent-end))))
	;; Non-block line: pad normally
	(org-modern-indent--add-props change-beg change-end line extra-pad)))
  (forward-line))

(defvar org-modern-indent-set-line-properties--orig
  (symbol-function 'org-indent-set-line-properties)
  "Original `org-indent-set-line-properties' function.")

(define-minor-mode org-modern-indent-mode
  "Org-modern with org-indent"
  :global nil
  :group 'org-modern
  (if org-modern-indent-mode
      (progn
	(setq org-modern-indent-begin
	      (propertize "╭" 'face 'org-meta-line)
	      org-modern-indent-guide
	      (propertize "│" 'face 'org-meta-line)
	      org-modern-indent-end
	      (propertize "╰" 'face 'org-meta-line))
	(setq-local org-fontify-quote-and-verse-blocks t)
	(setf (symbol-function 'org-indent-set-line-properties)
	      (symbol-function 'org-modern-indent-set-line-properties)))
    (setf (symbol-function 'org-indent-set-line-properties)
	  org-modern-indent-set-line-properties--orig))
  (org-indent-indent-buffer))

(provide 'org-modern-indent)
;;; org-modern-indent.el ends here
