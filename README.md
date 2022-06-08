# org-modern-indent
`org-modern` and `org-indent`, working together

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nice.  But when `org-indent` is enabled, the block "bracket", which uses the fringe, is disabled.  

This small package reproduces the block styling of `org-modern`, even when `org-indent` is enabled. 

<p align="center">
<img src=https://user-images.githubusercontent.com/93749/172438142-d4090856-dea8-43d0-a68a-bba29198575f.png>
</p>

## Notes

This package is only for users of `org-indent-mode`, and will enable indent if not set.  Non-zero `line-spacing` will introduce gaps between the block bracket characters.  Can be used _with or without_ `org-modern`. 

## Configure

Be sure to enable `org-indent` (see `org-startup-indented`).

```elisp
(use-package org-modern-indent
  :hook
  (org-modern-mode . org-modern-indent-mode))
```
