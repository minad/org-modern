;;; org-modern.el --- Modern looks for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/org-modern

;; This file is part of GNU Emacs.

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
(eval-when-compile
  (require 'cl-lib))

(defgroup org-modern nil
  "Modern looks for Org."
  :group 'org
  :prefix "org-modern-")

(defvar org-modern-label-border)
(defvar org-modern-variable-pitch)
(defun org-modern--update-label-face ()
  "Update border of the `org-modern-label' face."
  (when (facep 'org-modern-label)
    (set-face-attribute
     'org-modern-label nil
     :inherit (and org-modern-variable-pitch 'variable-pitch)
     :box (when org-modern-label-border
            (let ((border (if (eq org-modern-label-border 'auto)
                              (max 3 (cond
                                      ((integerp line-spacing) line-spacing)
                                      ((floatp line-spacing) (ceiling (* line-spacing (frame-char-height))))
                                      (t (/ (frame-char-height) 10))))
                            org-modern-label-border)))
              (list :color (face-attribute 'default :background nil t)
                    :line-width
                    ;; Emacs 28 supports different line horizontal and vertical line widths
                    (if (>= emacs-major-version 28)
                        (cons 0 (- border))
                      (- border))))))))

(defun org-modern--setter (sym val)
  "Set SYM to VAL and update faces."
  (set sym val)
  (org-modern--update-label-face))

(defcustom org-modern-label-border 'auto
  "Line width used for tag label borders.
If set to `auto' the border width is computed based on the `line-spacing'.
A value between 0.1 and 0.4 of `line-spacing' is recommended."
  :type '(choice (const nil) (const auto) integer)
  :set #'org-modern--setter)

(defcustom org-modern-star ["◉""○""◈""◇""⁕"]
  "Replacement strings for headline stars for each level.
Set to nil to disable styling the headlines."
  :type '(choice (const nil) (vector string)))

(defcustom org-modern-hide-stars 'leading
  "Make some of the headline stars invisible."
  :type '(choice
          (const :tag "Do not hide stars" nil)
          (const :tag "Hide all stars" t)
          (const :tag "Hide leading stars" leading)))

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

(defcustom org-modern-table-horizontal 0.1
  "Prettify horizontal table lines."
  :type '(choice (const nil) number))

(defcustom org-modern-priority t
  "Prettify priorities."
  :type 'boolean)

(defcustom org-modern-list
  '((?+ . "◦")
    (?- . "–")
    (?* . "•"))
  "List of bullet replacement strings.
Set to nil to disable styling list bullets."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-checkbox
  '((?X . #("▢✓" 0 2 (composition ((2)))))
    (?- . #("▢–" 0 2 (composition ((2)))))
    (?\s . #("▢" 0 1 (composition ((1))))))
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
  "Prettify keywords like #+title.
If set to a string, e.g., \"‣\", the string is used as replacement for #+."
  :type '(choice (boolean :tag "Hide keyword prefix")
                 (string :tag "Custom replacement")
                 (const :tag "Triangle bullet" "‣")))

(defcustom org-modern-statistics t
  "Prettify todo statistics."
  :type 'boolean)

(defcustom org-modern-progress ["○""◔""◐""◕""●"]
  "Add a progress indicator to the todo statistics.
Set to nil to disable the indicator."
  :type '(choice (const nil) (vector string)))

(defcustom org-modern-variable-pitch t
  "Prefer variable pitch for modern style."
  :type 'boolean
  :set #'org-modern--setter)

(defgroup org-modern-faces nil
  "Faces used by `org-modern'."
  :group 'org-modern
  :group 'org-faces
  :group 'faces)

(defface org-modern-label
  `((t :height 0.9 :width condensed :weight regular :underline nil))
  "Parent face for labels.")

(defface org-modern-block-keyword
  '((t :height 0.8 :weight light))
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

(defface org-modern-priority
  '((t :inherit (org-priority org-modern-label)
       :weight semibold :inverse-video t))
  "Face used for priority labels.")

(defface org-modern-statistics
  '((t :inherit org-modern-done))
  "Face used for todo statistics labels.")

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
  (let ((label (substring-no-properties (match-string 1))))
    (when org-modern-progress
      (let ((idx (floor
                  (* (1- (length org-modern-progress))
                     (if (match-beginning 2)
                         (* 0.01 (string-to-number (match-string 2)))
                       (let ((q (string-to-number (match-string 4))))
                         (if (= q 0)
                             1.0
                           (/ (* 1.0 (string-to-number (match-string 3))) q))))))))
        (setq label (concat (aref org-modern-progress idx) " " label))))
    (setq label (concat " " label " "))
    (add-text-properties (1- (match-beginning 1)) (1+ (match-end 1))
                         `(display ,label face org-modern-statistics))))

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
          (put-text-property
           colon
           (1+ colon)
           'display (format #(" %c" 1 3 (cursor t)) (char-after colon)))
          (put-text-property
           (- (point) 2)
           (1- (point))
           'display (format "%c " (char-before (1- (point)))))
          (put-text-property
           colon
           (1- (point))
           'face 'org-modern-tag))
        (setq colon (point))
        (add-text-properties (1- colon) colon colon-props)))))

(defun org-modern--todo ()
  "Prettify headline todo keywords."
  (let ((todo (match-string 1))
        (beg (match-beginning 1))
        (end (match-end 1)))
    (put-text-property
     beg (1+ beg)
     'display (format #(" %c" 1 3 (cursor t)) (char-after beg)))
    (put-text-property
     (1- end) end
     'display (format "%c " (char-before end)))
    (put-text-property
     beg end
     'face
     (if (member todo org-done-keywords)
         'org-modern-done
       'org-modern-todo))))

(defun org-modern--timestamp ()
  "Prettify timestamps."
  (let* ((active (eq (char-after (match-beginning 0)) ?<))
         (date-face (if active
                        'org-modern-date-active
                      'org-modern-date-inactive))
         (time-face (if active
                        'org-modern-time-active
                      'org-modern-time-inactive)))
    (put-text-property
     (match-beginning 0)
     (1+ (match-beginning 0))
     'display " ")
    (put-text-property
     (1- (match-end 0))
     (match-end 0)
     'display " ")
    ;; year-month-day
    (put-text-property
     (match-beginning 0)
     (if (eq (match-beginning 2) (match-end 2)) (match-end 0) (match-end 1))
     'face date-face)
    ;; hour:minute
    (unless (eq (match-beginning 2) (match-end 2))
      (put-text-property
       (1- (match-end 1))
       (match-end 1)
       'display (format "%c " (char-before (match-end 1))))
      (put-text-property
       (match-beginning 2)
       (match-end 0)
       'face time-face))))

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
           (tbeg (match-beginning 1))
           (tend (match-end 1))
           ;; Unique objects
           (sp1 (list 'space :width 1))
           (sp2 (list 'space :width 1))
           (color (face-attribute 'org-table :foreground nil t))
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
            (put-text-property a b 'display `(space :width (,org-modern-table-vertical))))
           (t (put-text-property a b 'face 'org-hide)))))
      (goto-char beg)
      (when separator
        (when (numberp org-modern-table-horizontal)
          (add-face-text-property tbeg tend `(:overline ,color) 'append)
          (add-face-text-property beg (1+ end) `(:height ,org-modern-table-horizontal) 'append))
        (while (re-search-forward "[^|+]+" tend 'noerror)
          (let ((a (match-beginning 0))
                (b (match-end 0)))
            ;; TODO Text scaling breaks the table formatting since the space is not scaled accordingly
            (cl-loop for i from a below b do
                     (put-text-property i (1+ i) 'display
                                        (if (= 0 (mod i 2)) sp1 sp2)))))))))


(define-fringe-bitmap 'org-modern--block-inner (make-vector 1 #x80) nil nil '(top t))
(define-fringe-bitmap 'org-modern--block-begin (vconcat (make-vector 20 0) [#xFF] (make-vector 107 #x80)) nil nil 'top)
(define-fringe-bitmap 'org-modern--block-end (vconcat (make-vector 107 #x80) [#xFF] (make-vector 20 0)) nil nil 'bottom)
(defun org-modern--block-fringe ()
  "Prettify blocks with fringe bitmaps."
  ;; Do not add source block fringe markers if org-indent-mode is
  ;; enabled. org-indent-mode uses line prefixes for indentation.
  ;; Therefore we cannot have both.
  (unless (bound-and-true-p org-indent-mode)
    (save-excursion
      (goto-char (match-beginning 0))
      (add-text-properties
       (point) (min (1+ (line-end-position)) (point-max))
       '(wrap-prefix
         #(" " 0 1 (display (left-fringe org-modern--block-begin org-block-begin-line)))
         line-prefix
         #(" " 0 1 (display (left-fringe org-modern--block-begin org-block-begin-line)))))
      (forward-line)
      (while
          (cond
           ((eobp) nil)
           ((save-excursion
              (let ((case-fold-search t))
                (re-search-forward
                 "^[ \t]*#\\+end_" (line-end-position) 'noerror)))
            (add-text-properties
             (point) (min (1+ (line-end-position)) (point-max))
             '(wrap-prefix
               #(" " 0 1 (display (left-fringe org-modern--block-end org-block-begin-line)))
               line-prefix
               #(" " 0 1 (display (left-fringe org-modern--block-end org-block-begin-line)))))
            nil)
           (t
            (add-text-properties
             (point) (min (1+ (line-end-position)) (point-max))
             '(wrap-prefix
               #(" " 0 1 (display (left-fringe org-modern--block-inner org-block-begin-line)))
               line-prefix
               #(" " 0 1 (display (left-fringe org-modern--block-inner org-block-begin-line)))))
            (forward-line)
            t))))))

;;;###autoload
(define-minor-mode org-modern-mode
  "Modern looks for Org."
  :global nil
  :group 'org-modern
  (cond
   (org-modern-mode
    (org-modern--update-label-face)
    (setq
     org-modern--keywords
     (append
      (when-let (bullet (alist-get ?+ org-modern-list))
        `(("^[ \t]*\\(+\\)[ \t]" 1 '(face nil display ,bullet))))
      (when-let (bullet (alist-get ?- org-modern-list))
        `(("^[ \t]*\\(-\\)[ \t]" 1 '(face nil display ,bullet))))
      (when-let (bullet (alist-get ?* org-modern-list))
        `(("^[ \t]+\\(*\\)[ \t]" 1 '(face nil display ,bullet))))
      (when org-modern-priority
        '(("^\\*+.*? \\(\\(\\[\\)#.\\(\\]\\)\\) "
           (1 'org-modern-priority t)
           (2 '(face nil display " "))
           (3 '(face nil display " ")))))
      (when org-modern-todo
        `((,(format "^\\*+ +%s " (regexp-opt org-todo-keywords-1 t)) (0 (org-modern--todo)))))
      (when org-modern-keyword
        `(("^[ \t]*\\(#\\+\\)\\S-" 1
           '(face nil
                  ,@(if (stringp org-modern-keyword)
                       `(display ,org-modern-keyword)
                     '(invisible t))))))
      (when org-modern-checkbox
        '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[[ X-]\\]\\)[ \t]"
           (0 (org-modern--checkbox)))))
      (when (or org-modern-star org-modern-hide-stars)
        `(("^\\(\\**\\)\\(\\*\\) "
           ,@(and (not (eq org-modern-hide-stars t)) org-modern-star '((0 (org-modern--star))))
           ,@(and (eq org-modern-hide-stars 'leading) '((1 '(face nil invisible t))))
           ,@(and (eq org-modern-hide-stars t) '((0 '(face nil invisible t)))))))
      (when org-modern-horizontal-rule
        '(("^-\\{5,\\}$" 0 '(face org-modern-horizontal-rule display (space :width text)))))
      (when org-modern-table
        '(("^[ \t]*\\(|.*|\\)[ \t]*$" (0 (org-modern--table)))))
      (when org-modern-block
        '(("^[ \t]*#\\+\\(?:begin\\|BEGIN\\)_\\S-" (0 (org-modern--block-fringe)))
          ("^\\([ \t]*#\\+\\(?:begin\\|BEGIN\\)_\\)\\(\\S-+\\).*"
           (1 '(face nil display (space :width (3))))
           (2 'org-modern-block-keyword append))
          ("^\\([ \t]*#\\+\\(?:end\\|END\\)_\\)\\(\\S-+\\).*"
           (1 '(face nil display (space :width (3))))
           (2 'org-modern-block-keyword append))))
      (when org-modern-tag
        '(("^\\*+.*?\\( \\)\\(:.*:\\)[ \t]*$" (0 (org-modern--tag)))))
      (when org-modern-timestamp
        '(("\\(?:<\\|\\[\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [[:word:]]+\\)?\\(?: [.+-]+[0-9ymwdh/]+\\)*\\)\\(\\(?: [0-9:-]+\\)?\\(?: [.+-]+[0-9ymwdh/]+\\)*\\)\\(?:>\\|\\]\\)"
           (0 (org-modern--timestamp)))
          ("<[^>]+>\\(-\\)\\(-\\)<[^>]+>\\|\\[[^]]+\\]\\(?1:-\\)\\(?2:-\\)\\[[^]]+\\]"
           (1 '(face org-modern-label display #("  " 1 2 (face (:strike-through t) cursor t))) t)
           (2 '(face org-modern-label display #("  " 0 1 (face (:strike-through t)))) t))))
      (when org-modern-statistics
        '((" \\[\\(\\([0-9]+\\)%\\|\\([0-9]+\\)/\\([0-9]+\\)\\)\\]" (0 (org-modern--statistics)))))))
    (font-lock-add-keywords nil org-modern--keywords 'append)
    (advice-add #'org-unfontify-region :after #'org-modern--unfontify))
   (t (font-lock-remove-keywords nil org-modern--keywords)
      (let ((org-modern-mode t))
        (org-modern--unfontify (point-min) (point-max)))))
  (font-lock-flush))

(defun org-modern--unfontify (beg end &optional _)
  "Unfontify prettified elements between BEG and END."
  (when org-modern-mode
    ;; TODO implement better unfontify
    (with-silent-modifications
      ;; Only remove line-prefix and wrap-prefix if org-indent-mode is disabled.
      (remove-list-of-text-properties
       beg end
       (if (bound-and-true-p org-indent-mode)
           '(display face invisible)
         '(wrap-prefix line-prefix display face invisible))))))

;;;###autoload
(defun org-modern-agenda ()
  "Finalize Org agenda highlighting."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((re (format " %s "
                        (regexp-opt
                         (append org-todo-keywords-for-agenda
                                 org-done-keywords-for-agenda) t)))
            (org-done-keywords org-done-keywords-for-agenda))
        (while (re-search-forward re nil 'noerror)
          (org-modern--todo))))))

(provide 'org-modern)
;;; org-modern.el ends here
