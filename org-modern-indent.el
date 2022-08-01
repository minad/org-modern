;;; org-modern-indent.el --- org-indent blocks like org-modern -*- lexical-binding: t; -*-
;; Copyright (C) 2022  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/org-modern-indent
;; Package-Requires: ((emacs "27.2") (org "9.5.2"))
;; Version: 0.0.2
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
;; even when org-indent is enabled.
;; Requires:
;;   - org-indent-mode enabled
;;   
;; Can be used with or without org-modern.   

;;; Code:
(require 'org-indent)
(require 'seq)

;; Add face for org-modern-indent line
(defface org-modern-indent-line '((t (:inherit (org-meta-line) :weight light)))
  "Face for line in org-modern-indent."
  :group 'faces)

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
(defvar-local org-modern--disabled nil)
(defun org-modern-indent-set-line-properties (level indentation &optional heading)
  "An org-modern inspired redefinition of `org-indent-set-line-properties'.
Used to approximate org-modern block style.  Treats blocks
specially, by extending the line and wrap prefixes with a box
guide unicode character."
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
    (unless (or org-modern--disabled (get-text-property (point) 'fontified)) 
      (font-lock-ensure change-beg line-end)) ;sometimes we beat font-lock
    (or (when-let (((eq heading nil))
		   ((not org-modern--disabled))
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

(defun org-modern-indent-block-insert (fun &rest r)
  "Refresh block after insertion.
To be set as :around advice for `org-insert-structure-template'."
  (let* ((reg (use-region-p))
	 (p (if reg (region-beginning) (point)))
	 (m (point-marker)))
    (set-marker-insertion-type m t)
    (if reg (set-marker m (region-end)))
    (let ((org-modern--disabled t)) (apply fun r))
    (org-indent-refresh-maybe p m nil)))

(defvar org-modern-indent-set-line-properties--orig
  (symbol-function 'org-indent-set-line-properties)
  "Original `org-indent-set-line-properties' function.")

(defgroup org-modern-indent nil
  "org-modern style blocks with org-indent."
  :group 'org
  :prefix "org-modern-indent")

;;;###autoload
(define-minor-mode org-modern-indent-mode
  "Org-modern with org-indent"
  :global nil
  :group 'org-modern-indent
  (if org-modern-indent-mode
      (progn
      (setq org-modern-indent-begin
	    (propertize "╭" 'face 'org-modern-indent-line)
	    org-modern-indent-guide
	    (propertize "│" 'face 'org-modern-indent-line)
	    org-modern-indent-end
	    (propertize "╰" 'face 'org-modern-indent-line))
      (setq-local org-fontify-quote-and-verse-blocks t)
      (setf (symbol-function 'org-indent-set-line-properties)
	    (symbol-function 'org-modern-indent-set-line-properties))
      (advice-add #'org-insert-structure-template :around #'org-modern-indent-block-insert))
    (advice-remove #'org-insert-structure-template #'org-modern-indent-block-insert)
    (setf (symbol-function 'org-indent-set-line-properties)
	  org-modern-indent-set-line-properties--orig))
  (org-indent-indent-buffer))

(provide 'org-modern-indent)
;;; org-modern-indent.el ends here
