# org-modern-indent
Modern block styling with `org-indent`.

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nice.  But when `org-indent` is enabled, the block "bracket", which uses the fringe area, is disabled.

This small package reproduces the block styling of `org-modern` when using `org-indent`:

<p align="center">
<img src=https://user-images.githubusercontent.com/93749/172964083-afafa737-3b54-4d9e-aaf0-9a4741fa085c.png>
</p>

## Notes

- This package is only for users of `org-indent-mode`, and will _enable_ org-indent if it is not already.  
- Can be used _with or without_ `org-modern`. 
- Non-zero `line-spacing` will introduce gaps between the block bracket characters.  

## Configure

Be sure to enable `org-indent` (see `org-startup-indented`).

```elisp
(use-package org-modern-indent
  ;; :straight or :load-path here, to taste
  :hook
  (org-indent-mode . org-modern-indent-mode))
```

The default `fixed-pitch` font (from which `org-meta-line` inherits) has line spacing >1.0 on some systems. This will introduce gaps _even if your default font is changed_, and `line-space` is nil.  To correct it, add: 

```elisp
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0) ; or whatever font family
```

Optionally, if you want to use org-modern too:

```elisp
(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
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
- [`org-bullets`](https://github.com/sabof/org-bullets/blob/master/org-bullets.el): Unicode heading bullet replacement.
- [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): Prettify headings and plain lists.
- [`nano-emacs`](https://github.com/rougier/nano-emacs): Beautiful and simple SVG-based theme for much of emacs, including org. 
