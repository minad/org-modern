;;; org-modern.el --- Modern looks for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.9
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds some styling to your Org buffer, which gives it a
;; modern look.  Enable the styling by default with:
;;   (add-hook 'org-mode-hook 'org-modern-mode)

;;; Code:

(require 'compat)
(require 'org)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup org-modern nil
  "Modern looks for Org."
  :link '(info-link :tag "Info Manual" "(org-modern)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/org-modern")
  :link '(emacs-library-link :tag "Library Source" "org-modern.el")
  :group 'org
  :prefix "org-modern-")

(defcustom org-modern-label-border 'auto
  "Line width used for tag label borders.
If set to `auto' the border width is computed based on the `line-spacing'.
A value between 0.1 and 0.4 of `line-spacing' is recommended."
  :type '(choice (const nil) (const auto) integer))

(defcustom org-modern-star '("◉" "○" "◈" "◇" "✳")
  "Replacement strings for headline stars for each level.
Set to nil to disable styling the headlines."
  :type '(repeat string))

(defcustom org-modern-hide-stars 'leading
  "Changes the displays of the stars.
Can be leading, t, or a string replacement for each leading star.
Set to nil to disable."
  :type '(choice
          (string :tag "Replacement string for leading stars")
          (const :tag "Do not hide stars" nil)
          (const :tag "Hide all stars" t)
          (const :tag "Hide leading stars" leading)))

(defcustom org-modern-timestamp t
  "Prettify time stamps, e.g. <2022-03-01>.
Set to nil to disable styling the time stamps.  In order to use
custom timestamps, the format should be (DATE . TIME) where DATE
is the format for date, and TIME is the format for time.  DATE
and TIME must be surrounded with space.  For the syntax, refer to
`format-time-string'."
  :type '(choice
          (const :tag "Disable time stamp styling" nil)
          (const :tag "Enable timestamp styling" t)
          (const :tag "Use format YYYY-MM-DD HH:MM" (" %Y-%m-%d " . " %H:%M "))
          (cons :tag "Custom format" string string)))

(defcustom org-modern-table t
  "Prettify tables."
  :type 'boolean)

(defcustom org-modern-table-vertical 3
  "Width of vertical table lines in pixels.
Set to nil to hide the vertical lines."
  :type '(choice (const nil) natnum))

(defcustom org-modern-table-horizontal 0.1
  "Prettify horizontal table lines."
  :type '(choice (const nil) number))

(defcustom org-modern-priority t
  "Prettify priorities.
If set to t, the priority will be prettified with the brackets
hidden.  If set to an alist of characters and strings, the
associated string will be used as replacement for the given
priority.  Example:

  (setq org-modern-priority
    (quote ((?A . \"❗\")
            (?B . \"⬆\")
            (?C . \"⬇\"))))"
  :type '(choice (boolean :tag "Prettify")
                 (alist :key-type (character :tag "Priority")
                        :value-type (string :tag "Replacement"))))

(defcustom org-modern-list
  '((?+ . "◦")
    (?- . "–")
    (?* . "•"))
  "List of bullet replacement strings.
Set to nil to disable styling list bullets."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-checkbox
  '((?X . "☑")
    (?- . #("□–" 0 2 (composition ((2)))))
    (?\s . "□"))
  "List of check box replacement strings.
Set to nil to disable styling checkboxes."
  :type '(alist :key-type character :value-type string))

(defcustom org-modern-horizontal-rule t
  "Prettify horizontal rulers.
The value can either be a boolean to enable/disable style or display
replacement expression, e.g., a string."
  :type '(choice boolean sexp))

(defcustom org-modern-todo t
  "Prettify todo keywords, see `org-todo-keywords'."
  :type 'boolean)

(defcustom org-modern-todo-faces nil
  "Faces for todo keywords.
This is an alist, with todo keywords in the car
and faces in the cdr.  Example:

  (setq org-modern-todo-faces
    (quote ((\"TODO\" :background \"red\"
                    :foreground \"yellow\"))))"
  :type '(repeat
          (cons (choice
                  (string :tag "Keyword")
                  (const :tag "Default" t))
                (sexp :tag "Face   "))))

(defcustom org-modern-priority-faces nil
  "Faces for priority tags.
This is an alist, with priority character in the car and faces in
the cdr.  Example:

  (setq org-modern-priority-faces
    (quote ((?A :background \"red\"
                :foreground \"yellow\"))))"
  :type '(repeat
          (cons (choice
                 (character :tag "Priority")
                 (const :tag "Default" t))
                (sexp :tag "Face   "))))

(defcustom org-modern-tag t
  "Prettify tags in headlines, e.g., :tag1:tag2:."
  :type 'boolean)

(defcustom org-modern-block-name t
  "Prettify blocks names, i.e. #+begin_NAME and #+end_NAME lines.
If set to a pair of two strings, e.g. (\"‣\" . \"‣\"), the strings are
used as replacements for the #+begin_ and #+end_ prefixes, respectively.
If set to an alist of block names and cons cells of strings, the associated
strings will be used as a replacements for the whole of #+begin_NAME and
#+end_NAME, respectively, and the association with t treated as the value for
all other blocks."
  :type '(choice
          (const :tag "Hide #+begin_ and #+end_ prefixes" t)
          (cons (string :tag "#+begin_ replacement")
                (string :tag "#+end_ replacement"))
          (const :tag "Triangle bullets" ("‣" . "‣"))
          (alist :key-type
                 (choice
                  (string :tag "Block name")
                  (const :tag "Default" t))
                 :value-type
                 (choice
                  (list (string :tag "#+begin_NAME replacement")
                        (string :tag "#+end_NAME replacement"))
                  (const :tag "Hide #+begin_ and #+end_ prefixes" t)))))

(defcustom org-modern-block-fringe 0
  "Add a border to the blocks in the fringe.
This variable can also be set to an integer between 0 and 16,
which specifies the offset of the block border from the edge of
the window."
  :type '(choice boolean natnum))

(defcustom org-modern-keyword t
  "Prettify keywords like #+title.
If set to t, the prefix #+ will be hidden.  If set to a string,
e.g., \"‣\", the string is used as replacement for #+.  If set to
an alist of keywords and strings, the associated string will be
used as replacement for \"#+keyword:\", with t the default key.
Example:

  (setq org-modern-keyword
    (quote ((\"options\" . \"🔧\")
            (t . t))))"
  :type '(choice (boolean :tag "Hide prefix")
                 (string :tag "Replacement")
                 (const :tag "Triangle bullet" "‣")
                 (alist :key-type (choice (string :tag "Keyword")
                                          (const :tag "Default" t))
                        :value-type (choice (string :tag "Replacement")
                                            (const :tag "Hide prefix" t)))))

(defcustom org-modern-footnote (cons nil (cadr org-script-display))
  "Prettify footnotes.
The car corresponds to display specification for definitions, the cdr for
references."
  :type '(choice (const nil) (cons sexp sexp)))

(defcustom org-modern-internal-target '(" ↪ " t " ")
  "Prettify internal link targets, e.g., <<introduction>>."
  :type '(choice (const nil) (list string boolean string)))

(defcustom org-modern-radio-target '(" ⛯ " t " ")
  "Prettify radio link targets, e.g., <<<radio>>>."
  :type '(choice (const nil) (list string boolean string)))

(defcustom org-modern-statistics t
  "Prettify todo statistics."
  :type 'boolean)

(defcustom org-modern-progress '("○" "◔" "◐" "◕" "●")
  "Add a progress indicator to the todo statistics.
Set to nil to disable the indicator."
  :type '(repeat string))

(defgroup org-modern-faces nil
  "Faces used by `org-modern'."
  :group 'org-modern
  :group 'org-faces
  :group 'faces)

(defface org-modern-symbol nil
  "Face used for stars, checkboxes and progress indicators.
You can specify a font `:family'.  The font families `Iosevka', `Hack' and
`DejaVu Sans' give decent results.")

(defface org-modern-label
  `((t :height 0.9 :width condensed :weight regular :underline nil))
  "Parent face for labels.
The parameters of this face depend on typographical properties of
the font and should therefore be adjusted by the user depending
on their font, e.g., the :width or :height parameters.  Themes
should not override this face, since themes usually don't control
the font.")

(defface org-modern-block-name
  '((t :height 0.8 :weight light))
  "Face used for block keywords.")

(defface org-modern-tag
  '((default :inherit (secondary-selection org-modern-label))
    (((background light)) :foreground "black")
    (t :foreground "white"))
  "Face used for tag labels.")

(defface org-modern-internal-target
  '((t :inherit org-modern-done))
  "Face used for internal link targets.")

(defface org-modern-radio-target
  '((t :inherit org-modern-done))
  "Face used for radio link targets.")

(defface org-modern-done
  '((default :inherit org-modern-label)
    (((background light)) :background "gray90" :foreground "black")
    (t :background "gray20" :foreground "white"))
  "Face used for done labels.")

(defface org-modern-todo
  ;; `:inverse-video' to use todo foreground as label background
  '((t :inherit (org-todo org-modern-label)
       :weight semibold :inverse-video t))
  "Default face used for todo labels.")

(defface org-modern-priority
  ;; `:inverse-video' to use priority foreground as label background
  '((t :inherit (org-priority org-modern-label)
       :weight semibold :inverse-video t))
  "Default face used for priority labels.")

(defface org-modern-statistics
  '((t :inherit org-modern-done))
  "Face used for todo statistics labels.")

(defface org-modern-date-active
  '((t :inherit org-modern-done))
  "Face used for active date labels.")

(defface org-modern-time-active
  ;; Use `:distant-foreground' to ensure readability if `hl-line-mode' is used.
  '((default :inherit org-modern-label :weight semibold)
    (((background light))
     :background "gray35" :foreground "white" :distant-foreground "black")
    (t :background "gray75" :foreground "black" :distant-foreground "white"))
  "Face used for active time labels.")

(defface org-modern-date-inactive
  '((default :inherit org-modern-label)
    (((background light))
     :background "gray90" :foreground "gray30")
    (t :background "gray20" :foreground "gray70"))
  "Face used for inactive date labels.")

(defface org-modern-time-inactive
  ;; Use `:distant-foreground' to ensure readability if `hl-line-mode' is used.
  '((default :inherit org-modern-label :background "gray50")
    (((background light)) :foreground "gray95" :distant-foreground "gray5")
    (t :foreground "gray5" :distant-foreground "gray95"))
  "Face used for inactive time labels.")

(defface org-modern-horizontal-rule
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face used for horizontal ruler.")

(defvar-local org-modern--font-lock-keywords nil)
(defvar-local org-modern--star-cache nil)
(defvar-local org-modern--hide-stars-cache nil)
(defvar-local org-modern--reset-hide-leading-stars nil)
(defvar-local org-modern--checkbox-cache nil)
(defvar-local org-modern--progress-cache nil)
(defvar-local org-modern--table-sp-width 0)
(defconst org-modern--table-overline '(:overline t))
(defconst org-modern--table-sp '((space :width (org-modern--table-sp-width))
                                 (space :width (org-modern--table-sp-width))))

(defun org-modern--checkbox ()
  "Prettify checkboxes according to `org-modern-checkbox'."
  (let ((beg (match-beginning 1))
        (end (match-end 1)))
    (put-text-property
     beg end 'display
     (cdr (assq (char-after (1+ beg)) org-modern--checkbox-cache)))))

(defun org-modern--keyword ()
  "Prettify keywords according to `org-modern-keyword'."
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        (rep (and (listp org-modern-keyword)
                  (cdr (assoc (downcase (match-string 2)) org-modern-keyword)))))
    (unless rep
      (setq rep (cdr (assq t org-modern-keyword)) end (match-end 1)))
    (pcase rep
      ('t (put-text-property beg (match-end 1) 'invisible 'org-modern))
      ((pred stringp)
       (put-text-property beg end 'display rep)))))

(defun org-modern--priority ()
  "Prettify priorities according to `org-modern-priority'."
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (prio (char-before (1- end))))
    (if-let ((rep (and (consp org-modern-priority)
                       (cdr (assq prio org-modern-priority)))))
        (put-text-property beg end 'display rep)
      (put-text-property beg (1+ beg) 'display " ")
      (put-text-property (1- end) end 'display " ")
      (put-text-property
       beg end 'face
       (if-let ((face (or (cdr (assq prio org-modern-priority-faces))
                          (cdr (assq t org-modern-priority-faces)))))
           `(:inherit (,face org-modern-label))
         'org-modern-priority)))))

(defun org-modern--progress ()
  "Prettify headline todo progress."
  (put-text-property
   (match-beginning 2) (match-end 2) 'display
   (aref org-modern--progress-cache
         (floor
          (* (1- (length org-modern--progress-cache))
             (if (match-beginning 3)
                 (* 0.01 (string-to-number (match-string 3)))
               (let ((q (string-to-number (match-string 5))))
                 (if (= q 0)
                     1.0
                   (/ (* 1.0 (string-to-number (match-string 4))) q)))))))))

(defun org-modern--tag ()
  "Prettify headline tags."
  (save-excursion
    (let* ((default-face (get-text-property (match-beginning 1) 'face))
           (colon-props `(display #(":" 0 1 (face org-hide)) face ,default-face))
           (beg (match-beginning 2))
           (end (match-end 2))
           colon-beg colon-end)
      (goto-char beg)
      (while (re-search-forward "::?" end 'noerror)
        (let ((cbeg (match-beginning 0))
              (cend (match-end 0)))
          (when colon-beg
            (put-text-property colon-end (1+ colon-end) 'display
                               (format #(" %c" 1 3 (cursor t)) (char-after colon-end)))
            (put-text-property (1- cbeg) cbeg 'display
                               (string (char-before cbeg) ?\s))
            (put-text-property colon-end cbeg 'face 'org-modern-tag))
          (add-text-properties cbeg cend colon-props)
          (setq colon-beg cbeg colon-end cend))))))

(defun org-modern--todo ()
  "Prettify headline todo keywords."
  (let ((todo (match-string 1))
        (beg (match-beginning 1))
        (end (match-end 1)))
    (put-text-property beg (1+ beg) 'display
                       (format #(" %c" 1 3 (cursor t)) (char-after beg)))
    (put-text-property (1- end) end 'display (string (char-before end) ?\s))
    (put-text-property
     beg end 'face
     (if-let ((face (or (cdr (assoc todo org-modern-todo-faces))
                        (cdr (assq t org-modern-todo-faces)))))
         `(:inherit (,face org-modern-label))
       (if (member todo org-done-keywords)
           'org-modern-done
         'org-modern-todo)))))

(defun org-modern--timestamp ()
  "Prettify timestamps."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (tbeg (match-beginning 2))
         (tend (match-end 2))
         (active (eq (char-after beg) ?<))
         (date-face (if active
                        'org-modern-date-active
                      'org-modern-date-inactive))
         (time-face (if active
                        'org-modern-time-active
                      'org-modern-time-inactive)))
    (remove-list-of-text-properties beg end '(display))
    (if (consp org-modern-timestamp)
        (let* ((time (save-match-data
                       (encode-time
                        (org-fix-decoded-time
                         (org-parse-time-string
                          (buffer-substring beg end))))))
               (fmt org-modern-timestamp)
               (date-str (format-time-string (car fmt) time))
               (time-str (format-time-string (cdr fmt) time)))
          ;; year-month-day
          (add-text-properties beg (if (eq tbeg tend) end tbeg)
                               `(face ,date-face display ,date-str))
          ;; hour:minute
          (unless (eq tbeg tend)
            (add-text-properties tbeg end
                                 `(face ,time-face display ,time-str))))
      (put-text-property beg (1+ beg) 'display " ")
      (put-text-property (1- end) end 'display " ")
      ;; year-month-day
      (put-text-property beg (if (eq tbeg tend) end tbeg) 'face date-face)
      ;; hour:minute
      (unless (eq tbeg tend)
        (put-text-property (1- tbeg) tbeg 'display
                           (string (char-before tbeg) ?\s))
        (put-text-property tbeg end 'face time-face)))))

(defun org-modern--star ()
  "Prettify headline stars."
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (level (- end beg)))
    (when (and org-modern--hide-stars-cache (not (eq beg end)))
      (cl-loop for i from beg below end do
               (put-text-property i (1+ i) 'display
                                  (nth (logand i 1)
                                       org-modern--hide-stars-cache))))
    (when org-modern-star
      (put-text-property
       (if (eq org-modern-hide-stars 'leading) beg end)
       (1+ end) 'display
       (aref org-modern--star-cache
             (min (1- (length org-modern--star-cache)) level))))))

(defun org-modern--table ()
  "Prettify vertical table lines."
  (save-excursion
    (let* ((beg (match-beginning 0))
           (end (match-end 0))
           (tbeg (match-beginning 1))
           (tend (match-end 1))
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
          (add-face-text-property tbeg tend org-modern--table-overline 'append)
          (add-face-text-property beg (1+ end) `(:height ,org-modern-table-horizontal) 'append))
        (while (re-search-forward "[^|+]+" tend 'noerror)
          (let ((a (match-beginning 0))
                (b (match-end 0)))
            (cl-loop for i from a below b do
                     (put-text-property i (1+ i) 'display
                                        (nth (logand i 1) org-modern--table-sp)))))))))

(defun org-modern--block-name ()
  "Prettify block according to `org-modern-block-name'."
  (let* ((beg-ind (match-beginning 1))
         (beg-rep (match-beginning 2))
         (end-rep (match-end 3))
         (beg-name (match-beginning 3))
         (end-name (match-end 3))
         (names (and (listp org-modern-block-name) org-modern-block-name))
         (rep (cdr (assoc (downcase (match-string 3)) names)))
         (fringe (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))))
    (unless rep
      (setq rep (cdr (assq t names)) end-rep beg-name))
    (when (consp rep)
      (setq rep (if (= 8 (- beg-name beg-rep)) (car rep) (cadr rep))))
    (pcase rep
      ('t
       (add-face-text-property beg-name end-name 'org-modern-block-name)
       (put-text-property (if fringe beg-ind beg-rep) beg-name 'invisible 'org-modern))
      ((pred stringp)
       (add-face-text-property beg-name end-name 'org-modern-block-name)
       (put-text-property beg-rep end-rep 'display rep)
       (when fringe
         (put-text-property beg-ind beg-rep 'invisible 'org-modern))))))

(defun org-modern--block-fringe ()
  "Prettify blocks with fringe bitmaps."
  (save-excursion
    (goto-char (match-beginning 0))
    (add-text-properties
     (point) (min (line-end-position) (point-max))
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
           (point) (min (line-end-position) (point-max))
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
          t)))))

(defun org-modern--pre-redisplay (_)
  "Compute font parameters before redisplay."
  (when-let ((box (and org-modern-label-border
                       (face-attribute 'org-modern-label :box nil t))))
    (unless (equal (and (listp box) (plist-get box :color))
                   (face-attribute 'default :background nil t))
      (org-modern--update-label-face)))
  (setf org-modern--table-sp-width (default-font-width)
        (cadr org-modern--table-overline) (face-attribute 'org-table :foreground nil t)))

(defun org-modern--update-label-face ()
  "Update border of the `org-modern-label' face."
  (set-face-attribute
   'org-modern-label nil
   :box
   (when org-modern-label-border
     (let ((border (if (eq org-modern-label-border 'auto)
                       (max 3 (cond
                               ((integerp line-spacing)
                                line-spacing)
                               ((floatp line-spacing)
                                (ceiling (* line-spacing (frame-char-height))))
                               (t (/ (frame-char-height) 10))))
                     org-modern-label-border)))
       (list :color (face-attribute 'default :background nil t)
             :line-width
             ;; Emacs 28 supports different line horizontal and vertical line widths
             (if (eval-when-compile (>= emacs-major-version 28))
                 (cons 0 (- border))
               (- border)))))))

(defun org-modern--update-fringe-bitmaps ()
  "Update fringe bitmaps."
  (when (and org-modern-block-fringe
             (fboundp 'fringe-bitmap-p)
             (not (fringe-bitmap-p 'org-modern--block-inner)))
    (let* ((g (ceiling (frame-char-height) 1.8))
           (h (- (default-line-height) g))
           (v (expt 2 (- 15 (if (booleanp org-modern-block-fringe) 0
                              org-modern-block-fringe))))
           (w (+ v v -1)))
      (define-fringe-bitmap 'org-modern--block-inner
        (vector v) nil 16 '(top t))
      (define-fringe-bitmap 'org-modern--block-begin
        (vconcat (make-vector g 0) (vector w) (make-vector (- 127 g) v)) nil 16 'top)
      (define-fringe-bitmap 'org-modern--block-end
        (vconcat (make-vector (- 127 h) v) (vector w) (make-vector h 0)) nil 16 'bottom))))

(defun org-modern--symbol (str)
  "Add `org-modern-symbol' face to STR."
  (setq str (copy-sequence str))
  (add-face-text-property 0 (length str) 'org-modern-symbol 'append str)
  str)

(defun org-modern--make-font-lock-keywords ()
  "Compute font-lock keywords."
  (append
   (when-let ((bullet (alist-get ?+ org-modern-list)))
     `(("^[ \t]*\\(+\\)[ \t]" 1 '(face nil display ,bullet))))
   (when-let ((bullet (alist-get ?- org-modern-list)))
     `(("^[ \t]*\\(-\\)[ \t]" 1 '(face nil display ,bullet))))
   (when-let ((bullet (alist-get ?* org-modern-list)))
     `(("^[ \t]+\\(*\\)[ \t]" 1 '(face nil display ,bullet))))
   (when org-modern-priority
     `(("^\\*+.*? \\(\\(\\[\\)#.\\(\\]\\)\\) "
        (1 (org-modern--priority)))))
   (when org-modern-todo
     `((,(format "^\\*+ +%s " (regexp-opt org-todo-keywords-1 t))
        (0 (org-modern--todo)))))
   (when org-modern-checkbox
     '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[[ X-]\\]\\)[ \t]"
        (0 (org-modern--checkbox)))))
   (when (or org-modern-star org-modern-hide-stars)
     `(("^\\(\\**\\)\\* "
        (0 ,(if (eq org-modern-hide-stars t)
                ''(face nil invisible org-modern)
              '(org-modern--star))))))
   (when org-modern-horizontal-rule
     `(("^[ \t]*-\\{5,\\}$" 0
        '(face org-modern-horizontal-rule display
               ,(if (eq org-modern-horizontal-rule t)
                    '(space :width text)
                  org-modern-horizontal-rule)))))
   (when org-modern-table
     '(("^[ \t]*\\(|.*|\\)[ \t]*$" (0 (org-modern--table)))))
   (when org-modern-tag
     `((,(concat "^\\*+.*?\\( \\)\\(:\\(?:" org-tag-re ":\\)+\\)[ \t]*$")
        (0 (org-modern--tag)))))
   (when org-modern-footnote
     `(("^\\(\\[fn:\\)[[:word:]-_]+\\]" ;; Definition
        ,@(if-let ((x (car org-modern-footnote)))
              `((0 '(face nil display ,x))
                (1 '(face nil display ,(propertize "[" 'display x))))
            '((1 '(face nil display "[")))))
       ("[^\n]\\(\\(\\[fn:\\)[[:word:]-_]+\\]\\)" ;; Reference
        ,@(if-let ((x (cdr org-modern-footnote)))
              `((1 '(face nil display ,x))
                (2 '(face nil display ,(propertize "[" 'display x))))
            '((2 '(face nil display "[")))))))
   (let ((target "\\([^<>\n\r\t ][^<>\n\r]*?[^<>\n\r\t @$]\\|[^<>\n\r\t @$]\\)"))
     (append
      (when org-modern-internal-target
        `((,(format "\\(<<\\)%s\\(>>\\)" target)
           (0 '(face org-modern-internal-target) t)
           (1 '(face nil display ,(org-modern--symbol (car org-modern-internal-target))))
           (3 '(face nil display ,(org-modern--symbol (caddr org-modern-internal-target))))
           ,@(unless (cadr org-modern-internal-target)
               '((2 '(face nil invisible org-modern)))))))
      (when org-modern-radio-target
        `((,(format "\\(<<<\\)%s\\(>>>\\)" target)
           (0 '(face org-modern-radio-target) t)
           (1 '(face nil display ,(org-modern--symbol (car org-modern-radio-target))))
           (3 '(face nil display ,(org-modern--symbol (caddr org-modern-radio-target))))
           ,@(unless (cadr org-modern-radio-target)
               '((2 '(face nil invisible org-modern)))))))))
   (when org-modern-timestamp
     '(("\\(?:<\\|\\[\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [[:word:]]+\\.?\\)?\\(?: [.+-]+[0-9ymwdh/]+\\)*\\)\\(\\(?: [0-9:-]+\\)?\\(?: [.+-]+[0-9ymwdh/]+\\)*\\)\\(?:>\\|\\]\\)"
        (0 (org-modern--timestamp)))
       ("<[^>]+>\\(-\\)\\(-\\)<[^>]+>\\|\\[[^]]+\\]\\(?1:-\\)\\(?2:-\\)\\[[^]]+\\]"
        (1 '(face org-modern-label display #("  " 1 2 (face (:strike-through t) cursor t))) t)
        (2 '(face org-modern-label display #("  " 0 1 (face (:strike-through t)))) t))))
   (when org-modern-statistics
     `((" \\(\\(\\[\\)\\(?:\\([0-9]+\\)%\\|\\([0-9]+\\)/\\([0-9]+\\)\\)\\(\\]\\)\\)"
        (1 '(face org-modern-statistics) t)
        (2 ,(if org-modern-progress '(org-modern--progress) ''(face nil display " ")))
        (6 '(face nil display " ")))))
   '((org-fontify-meta-lines-and-blocks)) ;; Ensure that blocks are properly fontified
   (when org-modern-keyword
     `(("^[ \t]*\\(#\\+\\)\\([^: \t\n]+\\):"
        ,@(pcase org-modern-keyword
            ('t '(1 '(face nil invisible org-modern)))
            ((pred stringp) `(1 '(face nil display ,org-modern-keyword)))
            (_ '(0 (org-modern--keyword)))))))
   ;; Do not add source block fringe markers if org-indent-mode is
   ;; enabled. org-indent-mode uses line prefixes for indentation.
   ;; Therefore we cannot have both.
   (when (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))
     '(("^[ \t]*#\\+\\(?:begin\\|BEGIN\\)_\\S-"
        (0 (org-modern--block-fringe)))))
   (when org-modern-block-name
     (let* ((indent (and org-modern-block-fringe
                         (not (bound-and-true-p org-indent-mode))
                         '((1 '(face nil invisible org-modern)))))
            (name '(3 'org-modern-block-name append))
            (hide `(,@indent (2 '(face nil invisible org-modern)) ,name))
            (specs
             (pcase org-modern-block-name
               ('t ;; Hide
                (cons hide hide))
               (`((,_k . ,_v) . ,_rest) ;; Dynamic replacement
                '(((0 (org-modern--block-name))) . ((0 (org-modern--block-name)))))
               (`(,beg . ,end) ;; Static replacement
                `((,@indent (2 '(face nil display ,beg)) ,name) .
                  (,@indent (2 '(face nil display ,end)) ,name))))))
       `(("^\\([ \t]*\\)\\(#\\+\\(?:begin\\|BEGIN\\)_\\)\\(\\S-+\\).*"
          ,@(car specs))
         ("^\\([ \t]*\\)\\(#\\+\\(?:end\\|END\\)_\\)\\(\\S-+\\).*"
          ,@(cdr specs)))))))

;;;###autoload
(define-minor-mode org-modern-mode
  "Modern looks for Org."
  :global nil
  :group 'org-modern
  (when org-modern--reset-hide-leading-stars
    (setq-local org-hide-leading-stars t
                org-modern--reset-hide-leading-stars nil))
  (cond
   (org-modern-mode
    (add-to-invisibility-spec 'org-modern)
    (when (and (eq org-modern-hide-stars 'leading) org-hide-leading-stars)
      (setq-local org-modern--reset-hide-leading-stars t
                  org-hide-leading-stars nil))
    (setq
     org-modern--star-cache
     (vconcat (mapcar #'org-modern--symbol org-modern-star))
     org-modern--hide-stars-cache
     (and (stringp org-modern-hide-stars)
          (list (org-modern--symbol org-modern-hide-stars)
                (org-modern--symbol org-modern-hide-stars)))
     org-modern--progress-cache
     (vconcat (mapcar
               (lambda (x) (concat " " (org-modern--symbol x) " "))
               org-modern-progress))
     org-modern--checkbox-cache
     (mapcar (pcase-lambda (`(,k . ,v)) (cons k (org-modern--symbol v)))
             org-modern-checkbox)
     org-modern--font-lock-keywords
     (append (remove '(org-fontify-meta-lines-and-blocks) org-font-lock-keywords)
             (org-modern--make-font-lock-keywords)))
    (font-lock-remove-keywords nil org-font-lock-keywords)
    (font-lock-add-keywords nil org-modern--font-lock-keywords)
    (setq-local font-lock-unfontify-region-function #'org-modern--unfontify)
    (add-hook 'pre-redisplay-functions #'org-modern--pre-redisplay nil 'local)
    (add-hook 'org-after-promote-entry-hook #'org-modern--unfontify-line nil 'local)
    (add-hook 'org-after-demote-entry-hook #'org-modern--unfontify-line nil 'local)
    (org-modern--update-label-face)
    (org-modern--update-fringe-bitmaps))
   (t
    (remove-from-invisibility-spec 'org-modern)
    (font-lock-remove-keywords nil org-modern--font-lock-keywords)
    (font-lock-add-keywords nil org-font-lock-keywords)
    (setq-local font-lock-unfontify-region-function #'org-unfontify-region)
    (remove-hook 'pre-redisplay-functions #'org-modern--pre-redisplay 'local)
    (remove-hook 'org-after-promote-entry-hook #'org-modern--unfontify-line 'local)
    (remove-hook 'org-after-demote-entry-hook #'org-modern--unfontify-line 'local)))
  (save-restriction
    (widen)
    (with-silent-modifications
      (org-modern--unfontify (point-min) (point-max)))
    (font-lock-flush)))

(defun org-modern--unfontify-line ()
  "Unfontify prettified elements on current line."
  (org-modern--unfontify (pos-bol) (pos-eol)))

(defun org-modern--unfontify (beg end &optional _loud)
  "Unfontify prettified elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append
          ;; Only remove line-prefix and wrap-prefix if org-indent-mode is disabled.
          (if (bound-and-true-p org-indent-mode)
              '(display invisible)
            '(wrap-prefix line-prefix display invisible))
          font-lock-extra-managed-props)))
    (org-unfontify-region beg end)))

;;;###autoload
(defun org-modern-agenda ()
  "Finalize Org agenda highlighting."
  (remove-from-invisibility-spec 'org-modern)
  (add-to-invisibility-spec 'org-modern) ;; Not idempotent?!
  (add-hook 'pre-redisplay-functions #'org-modern--pre-redisplay nil 'local)
  (save-excursion
    (save-match-data
      (when org-modern-todo
        (goto-char (point-min))
        (let ((re (format " %s "
                          (regexp-opt
                           (append org-todo-keywords-for-agenda
                                   org-done-keywords-for-agenda) t)))
              (org-done-keywords org-done-keywords-for-agenda))
          (while (re-search-forward re nil 'noerror)
            (org-modern--todo))))
      (when org-modern-tag
        (goto-char (point-min))
        (let ((re (concat "\\( \\)\\(:\\(?:" org-tag-re "::?\\)+\\)[ \t]*$")))
          (while (re-search-forward re nil 'noerror)
            (org-modern--tag))))
      (when org-modern-priority
        (goto-char (point-min))
        (while (re-search-forward "\\(\\[#.\\]\\)" nil 'noerror)
          ;; For some reason the org-agenda-fontify-priorities adds overlays?!
          (when-let ((ov (overlays-at (match-beginning 0))))
            (overlay-put (car ov) 'face nil))
          (org-modern--priority))))))

;;;###autoload
(define-globalized-minor-mode global-org-modern-mode
  org-modern-mode org-modern--on
  :group 'org-modern
  (if global-org-modern-mode
      (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    (remove-hook 'org-agenda-finalize-hook #'org-modern-agenda)))

(defun org-modern--on ()
  "Enable `org-modern' in every Org buffer."
  (when (derived-mode-p #'org-mode)
    (org-modern-mode)))

(provide 'org-modern)
;;; org-modern.el ends here
