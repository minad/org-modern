# org-modern-indent
Modern block styling with `org-indent`.

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nice.  But when `org-indent` is enabled, the block "bracket", which uses the fringe area, is disabled.

This small package reproduces the block styling of `org-modern` when using `org-indent`:

<p align="center">
<img src=https://user-images.githubusercontent.com/93749/172438142-d4090856-dea8-43d0-a68a-bba29198575f.png>
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
  (org-mode . org-modern-indent-mode))
```

## Related packages

- [`org-modern`](https://github.com/minad/org-modern): A modern org styling.  Works best without org-indent.
- [`org-bullets`](https://github.com/sabof/org-bullets/blob/master/org-bullets.el): Unicode heading bullet replacement.
- [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): Prettify headings and plain lists.
- [`nano-emacs`](https://github.com/rougier/nano-emacs): Beautiful and simple SVG-based theme for much of emacs, including org. 
