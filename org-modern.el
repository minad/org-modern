;;; org-modern.el --- Modern looks for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Mendler

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/org-modern

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds some styling to your Org buffer, which gives it a
;; modern look. Enable the styling by default with:
;;   (add-hook 'org-mode-hook 'org-modern-mode)

;;; Code:

(require 'org)

(defgroup org-modern nil
  "Modern looks for Org."
  :group 'org
  :prefix "org-modern-")

(defcustom org-modern-line-spacing 7
  "Line spacing, should approximately match the box line width."
  :type '(choice (const nil) integer))

(defcustom org-modern-star ["◉""○""◈""◇""⁕"]
  "Replacement strings for headline stars for each level.
Set to nil to disable styling the headlines."
  :type '(choice (const nil) (vector string)))

(defcustom org-modern-timestamp t
  "Prettify time stamps, e.g. <2022-03-01>.
Set to nil to disable styling the time stamps."
  :type 'boolean)

(defcustom org-modern-table t
  "Prettify tables."
  :type 'boolean)

(defcustom org-modern-table-vertical 3
  "Width of vertical table lines in pixels.
Set to nil to hide the vertical lines."
  :type '(choice (const nil) integer))

(defcustom org-modern-table-horizontal t
  "Prettify horizontal lines, consisting of five dashes or more."
  :type 'boolean)

(defcustom org-modern-priority
  '((?A . "🅐") ;; Ⓐ
    (?B . "🅑") ;; Ⓑ
    (?C . "🅒")) ;; Ⓒ
  "List of priority label replacements.
Set to nil to disable styling priorities."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-list
  '((?+ . "◦")
    (?- . "─")
    (?* . "•"))
  "List of bullet replacement strings.
Set to nil to disable styling list bullets."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-checkbox
  '((?X . "☑")
    (?- . "◫")
    (?\s . "☐"))
  "List of check box replacement strings.
Set to nil to disable styling checkboxes."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-horizontal-rule t
  "Prettify horizontal rulers."
  :type 'boolean)

(defcustom org-modern-todo t
  "Prettify todo keywords, see `org-todo-keywords'."
  :type 'boolean)

(defcustom org-modern-tag t
  "Prettify tags in headlines, e.g., :tag1:tag2:."
  :type 'boolean)

(defcustom org-modern-block t
  "Prettify blocks, wrapped by #+begin and #+end keywords."
  :type 'boolean)

(defcustom org-modern-keyword t
  "Prettify keywords like #+title."
  :type 'boolean)

(defcustom org-modern-statistics t
  "Prettify todo statistics."
  :type 'boolean)

(defcustom org-modern-progress " ▁▂▃▄▅▆▇█"
  "Add a progress bar to the todo statistics.
Set to nil to disable the progress bar."
  :type '(choice (const nil) string))

(defgroup org-modern-faces nil
  "Faces used by `org-modern'."
  :group 'org-modern
  :group 'org-faces
  :group 'faces)

(defface org-modern-label
  `((t :height 0.75
       :inherit variable-pitch
       :width condensed :weight regular
       :underline nil
       ,@(and org-modern-line-spacing
              `(:box (:line-width ,(- org-modern-line-spacing))))))
  "Parent face for labels.")

(defface org-modern-block-keyword
  '((t :height 0.75 :weight light))
  "Face used for block keywords.")

(defface org-modern-tag
  '((default :inherit (secondary-selection org-modern-label))
    (((background light)) :foreground "black")
    (t :foreground "white"))
  "Face used for tag labels.")

(defface org-modern-done
  '((default :inherit org-modern-label)
    (((background light)) :background "gray90" :foreground "black")
    (t :background "gray20" :foreground "white"))
  "Face used for done labels.")

(defface org-modern-todo
  '((t :inherit (org-todo org-modern-label)
       :weight semibold :inverse-video t))
  "Face used for todo labels.")

(defface org-modern-statistics
  '((t :inherit org-modern-done))
  "Face used for todo statistics labels.")

(defface org-modern-progress
  '((((background light)) :foreground "gray40")
    (t :foreground "gray60"))
  "Face used for todo statistics progress bar.")

(defface org-modern-date-active
  '((t :inherit org-modern-done))
  "Face used for active date labels.")

(defface org-modern-time-active
  '((default :inherit org-modern-label :weight semibold)
    (((background light))
     :background "gray35" :foreground "white")
    (t :background "gray75" :foreground "black"))
  "Face used for active time labels.")

(defface org-modern-date-inactive
  '((default :inherit org-modern-label)
    (((background light))
     :background "gray90" :foreground "gray30")
    (t :background "gray20" :foreground "gray70"))
  "Face used for inactive date labels.")

(defface org-modern-time-inactive
  '((default :inherit org-modern-label :background "gray50")
    (((background light)) :foreground "gray95")
    (t :foreground "gray5"))
  "Face used for inactive time labels.")

(defface org-modern-horizontal-rule
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face used for horizontal ruler.")

(defvar-local org-modern--keywords nil
  "List of font lock keywords.")

(defvar-local org-modern--orig-line-spacing 'unset
  "Original line spacing.")

(defun org-modern--priority ()
  "Prettify headline priorities using the `org-modern-priority' character."
  (let ((beg (match-beginning 1))
        (end (match-end 1)))
    (when-let (sym (alist-get (char-after (+ 2 beg)) org-modern-priority))
      (put-text-property beg end 'display sym))))

(defun org-modern--checkbox ()
  "Prettify checkboxes according to `org-modern-checkbox'."
  (let ((beg (match-beginning 1))
        (end (match-end 1)))
    (put-text-property
     beg end
     'display (alist-get (char-after (1+ beg))
                         org-modern-checkbox))))

(defun org-modern--statistics ()
  "Prettify headline todo statistics."
  (let ((label (propertize (match-string 1) 'face 'org-modern-statistics)))
    (when org-modern-progress
      (let ((idx (ceiling
                  (* (1- (length org-modern-progress))
                     (if (match-beginning 2)
                         (* 0.01 (string-to-number (match-string 2)))
                       (let ((q (string-to-number (match-string 4))))
                         (if (= q 0)
                             1.0
                           (/ (* 1.0 (string-to-number (match-string 3))) q))))))))
        (setq label (concat
                     (propertize (char-to-string (aref org-modern-progress idx))
                                 'face 'org-modern-progress)
                     " "
                     label))))
    (setq label (concat " " label " "))
    (add-face-text-property 0 (length label)
                            'org-modern-statistics 'append label)
    (put-text-property (1- (match-beginning 1)) (1+ (match-end 1))
                       'display label)))

(defun org-modern--tag ()
  "Prettify headline tags."
  (save-excursion
    (let* ((default-face (get-text-property (match-beginning 1) 'face))
           (colon-props `(display #(":" 0 1 (face org-hide)) face ,default-face))
           (beg (match-beginning 2))
           (end (match-end 2))
           colon)
      (goto-char beg)
      (while (search-forward ":" end 'noerror)
        (when colon
          (when (/= beg (1- colon))
            (add-face-text-property (1- colon) colon '(:height 1) t))
          (put-text-property
           colon
           (1- (point))
           'display
           (propertize (concat " " (buffer-substring colon (1- (point))) " ")
                       'face 'org-modern-tag)))
        (setq colon (point))
        (add-text-properties (1- colon) colon colon-props)))))

(defun org-modern--todo ()
  "Prettify headline todo keywords."
  (let ((todo (match-string 1))
        (beg (match-beginning 1))
        (end (match-end 1)))
    (put-text-property
     beg end
     'display
     (propertize (concat " " todo " ")
                 'face
                 (if (member todo org-done-keywords)
                     'org-modern-done
                   'org-modern-todo)))))

(defun org-modern--timestamp ()
  "Prettify timestamps."
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        (active (eq (char-after (match-beginning 0)) ?<))
        (date (match-string 1))
        (time (match-string 2)))
    (put-text-property
     beg end
     'display
     (concat
      (propertize (concat " " date " ")
                  'face
                  (if active
                      'org-modern-date-active
                    'org-modern-date-inactive))
      (and time
           (propertize (concat " " time " ")
                       'face
                       (if active
                           'org-modern-time-active
                         'org-modern-time-inactive)))))))

(defun org-modern--star ()
  "Prettify headline stars."
  (let ((level (- (match-end 1) (match-beginning 1))))
    (put-text-property
     (match-beginning 2)
     (match-end 2)
     'display
     (aref org-modern-star (min (1- (length org-modern-star)) level)))))

(defun org-modern--table ()
  "Prettify vertical table lines."
  (save-excursion
    (let* ((beg (match-beginning 0))
           (end (match-end 0))
           (inner (progn
                    (goto-char beg)
                    (forward-line)
                    (re-search-forward "^[ \t]*|" (line-end-position) t)))
           (separator (progn
                        (goto-char beg)
                        (re-search-forward "^[ \t]*|-" end 'noerror))))
      (goto-char beg)
      (while (re-search-forward
              "-+\\(?1:+\\)-\\|\\(?:^\\|[- ]\\)\\(?1:|\\)\\(?:$\\|[- ]\\)"
              end 'noerror)
        (let ((a (match-beginning 1))
              (b (match-end 1)))
          (cond
           ((and org-modern-table-vertical (or (not separator) inner))
            (add-text-properties a b
                                 `(display (space :width (,org-modern-table-vertical))
                                           face (:inherit org-table :inverse-video t))))
           ((and org-modern-table-horizontal separator)
            (put-text-property a b 'display `(space :width (,org-modern-table-vertical)))
            (add-face-text-property a b '(:overline t) 'append))
           (t (put-text-property a b 'face 'org-hide)))))
      (goto-char beg)
      (when separator
        (add-face-text-property beg (1+ end) '(:height 1) 'append)
        (while (re-search-forward "-+" end 'noerror)
          (let ((a (match-beginning 0))
                (b (match-end 0)))
            ;; TODO Text scaling breaks the table formatting since the space is not scaled accordingly
            (put-text-property a b 'display `(space :width ,(- b a)))
            (when org-modern-table-horizontal
              (add-face-text-property a b '(:overline t) 'append))))))))

(defun org-modern--block ()
  "Prettify blocks."
  (save-excursion
    (goto-char (match-beginning 0))
    (forward-line)
    (while (not (or (eobp)
                    (save-excursion
                      (re-search-forward
                       "^[ \t]*#\\+end_" (line-end-position) 'noerror))))
      (put-text-property
       (point) (1+ (point)) 'line-prefix
       #(" " 0 1 (display (left-fringe org-modern--line org-block-begin-line))))
      (forward-line))))

;;;###autoload
(define-minor-mode org-modern-mode
  "Modern looks for Org."
  :global nil
  :group 'org-modern
  (unless (eq org-modern--orig-line-spacing 'unset)
    (setq line-spacing org-modern--orig-line-spacing
          org-modern--orig-line-spacing 'unset))
  (cond
   (org-modern-mode
    (when-let (width (plist-get (face-attribute 'org-modern-label :box) :line-width))
      (set-face-attribute
       'org-modern-label nil
       :box `(:color ,(face-attribute 'default :background nil t) :line-width ,width)))
    (setq
     org-modern--orig-line-spacing line-spacing
     line-spacing org-modern-line-spacing
     org-modern--keywords
     (append
      (when-let (bullet (alist-get ?+ org-modern-list))
        `(("^[ \t]*\\(+\\)[ \t]" 1 '(face nil display ,bullet))))
      (when-let (bullet (alist-get ?- org-modern-list))
        `(("^[ \t]*\\(-\\)[ \t]" 1 '(face nil display ,bullet))))
      (when-let (bullet (alist-get ?* org-modern-list))
        `(("^[ \t]+\\(*\\)[ \t]" 1 '(face nil display ,bullet))))
      (when org-modern-priority
        `((,(format "^\\*+.*? \\(\\[#[%s]\\]\\) "
                    (apply #'string (mapcar #'car org-modern-priority)))
           (0 (org-modern--priority)))))
      (when org-modern-todo
        `((,(format "^\\*+ +%s " (regexp-opt org-todo-keywords-1 t)) (0 (org-modern--todo)))))
      (when org-modern-keyword
        `(("^[ \t]*\\(#\\+\\)\\S-" 1 '(face nil invisible t))))
      (when org-modern-checkbox
        '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[[ X-]\\]\\)[ \t]"
           (0 (org-modern--checkbox)))))
      (when org-modern-star
        '(("^\\(\\**\\)\\(\\*\\) " (0 (org-modern--star)))))
      (when org-modern-horizontal-rule
        '(("^-\\{5,\\}$" 0 '(face org-modern-horizontal-rule display (space :width text)))))
      (when org-modern-table
        '(("^[ \t]*|.*|[ \t]*$" (0 (org-modern--table)))))
      (when org-modern-block
        '(("^[ \t]*#\\+begin_\\S-" (0 (org-modern--block)))
          ("^\\([ \t]*#\\+begin_\\)\\(\\S-+\\)"
           (2 'org-modern-block-keyword append)
           (1 '(face nil display (space :width (3))
                     line-prefix #("│" 0 1 (display (left-fringe org-modern--top org-block-begin-line))))))
          ("^\\([ \t]*#\\+end_\\)\\(\\S-+\\)"
           (2 'org-modern-block-keyword append)
           (1 '(face nil display (space :width (3))
                     line-prefix #("│" 0 1 (display (left-fringe org-modern--bottom org-block-begin-line))))))))
      (when org-modern-tag
        '(("^\\*+.*?\\( \\)\\(:.*:\\)[ \t]*$" (0 (org-modern--tag)))))
      (when org-modern-timestamp
        '(("\\(?:<\\|\\[\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [A-Za-z]+\\)?\\)\\(?: \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)?\\(?:>\\|\\]\\)"
           (0 (org-modern--timestamp)))))
      (when org-modern-statistics
        '((" \\[\\(\\([0-9]+\\)%\\|\\([0-9]+\\)/\\([0-9]+\\)\\)\\]" (0 (org-modern--statistics)))))))
    (font-lock-add-keywords nil org-modern--keywords 'append)
    (advice-add #'org-unfontify-region :after #'org-modern--unfontify))
   (t (font-lock-remove-keywords nil org-modern--keywords)
      ;; TODO implement better unfontify
      (with-silent-modifications
        (remove-list-of-text-properties (point-min) (point-max)
                                        '(line-prefix display face invisible)))))
  (font-lock-flush))

(defun org-modern--unfontify (beg end &optional _)
  "Unfontify prettified elements between BEG and END."
  (when org-modern-mode
    ;; TODO implement better unfontify
    (remove-list-of-text-properties beg end '(line-prefix display face invisible))))

(define-fringe-bitmap 'org-modern--line (make-vector 1 #x80) nil nil '(top t))
(define-fringe-bitmap 'org-modern--top (vconcat (make-vector 20 0) [#xFF] (make-vector 107 #x80)) nil nil 'top)
(define-fringe-bitmap 'org-modern--bottom (vconcat (make-vector 107 #x80) [#xFF] (make-vector 20 0)) nil nil 'bottom)

(provide 'org-modern)
;;; org-modern.el ends here
