# org-modern-indent

Modern block styling with `org-indent`.

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nicely decorated.  But when `org-indent` is enabled, the block "bracket", which uses the fringe area, is disabled.

This small package approximately reproduces the block styling of `org-modern` when using `org-indent`.  It can be used with or without `org-modern`.  Recent versions support "bulk-indented" blocks nested within lists:

<img align="center" width=700 src=https://user-images.githubusercontent.com/93749/224204382-091fcd76-3ad0-467e-9525-287ae80e93c6.png>

## Updates

- v0.1 features a complete re-write to use font-lock directly.  This has a few benefits: 
  1. No longer relies on org-mode face names for recognizing
     blocks, so `org-src-block-faces` can have arbitrary faces
     applied, e.g. for different `src` languages, as in the screenshot.
  2. Eliminates the "race" between font-locking and applying the prefix text properties.
  3. Enables in-text bracket decorations for "bulk-indented" blocks, for example blocks situated
     in an arbitrarily-nested plain list item.

## Configure

```elisp
(use-package org-modern-indent
  :load-path "~/code/emacs/org-modern-indent/"
  ; or
  ; :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent"))
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))
```
Also, be sure to enable `org-indent` (see the variable `org-startup-indented`).

## Hints

### Bulk-indented blocks (e.g. within lists):

Bulk-indented blocks can have "real" (space/tab) indent applied and managed by org.  This extra indentation is appled by org on _top_ of the (fake, prefix-based) indentation used by org-indent.  To nest blocks within such indented content, e.g. in plain list items, you only have to begin the `#+begin` at the same column as the list element's text.  To help achieve this, here are a few ways to move blocks around in terms of their indentation:

- **Start things right**: Hit return after your last line of text (e.g in a list item), then immediately hit `C-c C,` to create the desired block.  It will be indented at the right level.
- **Move flush left** `M-{` gets you to the start of a block quickly.  `M-\` there block will move the blocks first header to the very left.  Then `M-S-left` (or `right`) will indent the full block.  `org-src-preserve-indentation=t` will help with indenting `example` blocks.
- **Indent rigidly** `M-h` selects the entire block. Then `C-x TAB` enters "rigid indent" mode, where left/right moves the entire block.

### Font spacing

The default `fixed-pitch` font (from which `org-meta-line` inherits) has line spacing >1.0 on some systems. This will introduce gaps _even if your default font is changed_, and `line-space` is nil.  To correct it, add: 

```elisp
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0) ; or whatever font family
```
### The bracket style 

If you'd like a different face than `org-meta-line` for the "bracket", configure the `org-modern-bracket-line` face.

### Related config

Optionally, if you want to use org-modern too:

```elisp
(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-table nil)
  (org-modern-list 
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))
```

Also optional; use org-bullets instead for nicely aligned bullet stars. 

```elisp
(use-package org-bullets-mode
  :ensure org-bullets
  :config
  :hook org-mode)
```


## Related packages

- [`org-modern`](https://github.com/minad/org-modern): A modern org styling.  Works best without org-indent.
- [`org-bullets`](https://github.com/sabof/org-bullets): Unicode heading bullet replacement.
- [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): Prettify headings and plain lists.
- [`nano-emacs`](https://github.com/rougier/nano-emacs): Beautiful and simple SVG-based theme for much of emacs, including org. 
