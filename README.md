# org-modern-indent
org-modern and org-indent, working together

[org-modern](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nice.  But when `org-indent` is enabled, the block "bracket", based on the fringe, no longer functions.  This small package reproduces the block styling, even with `org-indent`. 

## Configure

Be sure to enable `org-indent` (see `org-startup-indented`).

```elisp
(use-package org-modern-indent
  :hook
  (org-modern-mode . org-modern-indent-mode))
```
