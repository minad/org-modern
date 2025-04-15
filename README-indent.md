# org-modern-indent

Modern block styling with `org-indent`.

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nicely decorated.  But when `org-indent` is enabled, the block "bracket", which uses the fringe area, is disabled.

This small package reproduces the block styling of `org-modern` when using `org-indent`. Recent versions support "bulk-indented" blocks nested within lists:

<img width="716" alt="image" src="https://github.com/user-attachments/assets/7ca42ce7-dcfb-4c66-b5f4-1798a4fd4df5" />

>[!NOTE]
> Non-nil `line-spacing` is not recommended with this extension, as it leads to gaps in the vertical bars drawn to indicate blocks.

## Updates
- **v0.6**: Merged as an `org-modern` extension.
- **v0.5.1**: Small simplifications for block drawing.
- **v0.5**: Another complete rewrite which substantially improves
  performance and accuracy.  Now block detection uses `org-element`
  and the block styling is implemented in
  `before/after-change-functions`. Benefits include:
  1. Higher performance and more reliable fontification.
  2. Ability to detect and correctly treat _damaged_ blocks
     (header/footer line altered or removed) as well as _merged_ blocks.
  2. Caches all prefix strings for lower memory usage/GC churn.
  3. No more "runaway" formatting when partial blocks are created:
     only _real_ blocks (according to `org-element`) are fontified.
  
  Note that v0.5 implements indented block styling using display
  properties on the indentation text, so navigation will "skip over"
  it.
- **v0.1**: features a complete re-write to use font-lock directly.  This
  has a few benefits:
  1. No longer relies on org-mode face names for recognizing
     blocks, so `org-src-block-faces` can have arbitrary faces
     applied, e.g. for different `src` languages, as in the screenshot.
  2. Eliminates the "race" between font-locking and applying the prefix text properties.
  3. Enables in-text bracket decorations for "bulk-indented" blocks, for example blocks situated
     in an arbitrarily-nested plain list item.

>[!IMPORTANT]
> `org-modern-indent` uses `org-indent`, and expects it to be enabled to achieve its formatting.  To activate `org-indent-mode` by default in all org files, set `org-startup-indented=t`.

## Hints

### Bulk-indented blocks (e.g. within lists):

Bulk-indented blocks have "real" (space/tab) indent applied and managed by org.  This extra indentation is applied by org on _top_ of the (fake, prefix-based) indentation used by org-indent.  To nest blocks properly within such indented content, e.g. in plain list items, you only have to begin the `#+begin` at the same level as the list element's text.

As an important principle, `org-modern-indent` does not alter the contents of the text in your org documents, not even indentation.  It just styles what is there.  To help achieve proper block bulk-indented alignment, here are a few ways to alter blocks indentation using org and other commands:

- **Start things right**: Hit return after your last line of text (e.g in a list item), then immediately hit `C-c C,` to create the desired block.  It will be indented at the right level:
   ```org
   - This list item contains a:
       - sublist, which holds a block:
	     [C-c C-,] here
   ```
- **Move flush left**: Note: `M-{` will get you to the start of a block quickly.  `M-\` at block start will move the block's first header line to column 0.  Then `M-S-left` (or `right`) will indent the full block.
- **Indent rigidly**: `M-h` selects the entire block. Then `C-x TAB` enters "rigid indent" mode, after which left/right moves the entire block.
- **Re-indent a block**: If you have a block that is partially aligned, perhaps with a "hanging end", like so:
   ```org
   - List 1
       - List 2
	     #+begin_src lang
		   foo_lang(x)
	   #+end_src
   ```
  you can simply use `M-S-left/right` at block start (or in fact anywhere on the block header/footer) to `org-indent-block`.  Note that `org-src-preserve-indentation=nil` is an important setting, to allow org to (re-)indent blocks to respect the local indentation inside list and other elements.  Also note that (from `org-indent-region`): 

    > **Note**
    > The function will not indent contents of example blocks, verse blocks and export blocks as leading white spaces are assumed to be significant there.

### Font spacing

The default `fixed-pitch` font (from which `org-meta-line` inherits) has line spacing >1.0 on some systems. This will introduce gaps _even if your default font is changed_, and `line-space` is nil.  To correct it, add: 

```elisp
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0) ; or whatever font family
```
### The bracket style 

If you'd like a different face than `org-meta-line` for the "bracket", configure the `org-modern-indent-bracket-line` face.

### Related configuration

Optional; use org-bullets instead for nicely aligned bullet stars. 

```elisp
(use-package org-bullets-mode
  :ensure org-bullets
  :config
  :hook org-mode)
```

## Related packages

- [`org-bullets`](https://github.com/sabof/org-bullets): Unicode heading bullet replacement.
- [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): Prettify headings and plain lists.
