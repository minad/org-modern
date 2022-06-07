# org-modern-indent
`org-modern` and `org-indent`, working together

[`org-modern`](https://github.com/minad/org-modern) provides a clean and efficient org style.  The blocks (e.g. source, example) are particularly nice.  But when `org-indent` is enabled, the block "bracket", which uses the fringe, is disabled.  

This small package reproduces the block styling of `org-modern` when `org-indent` is enabled. 

![image](https://user-images.githubusercontent.com/93749/172436967-36e69e5a-9a58-46c4-875b-a9d6b829f38e.png)

## Configure

Be sure to enable `org-indent` (see `org-startup-indented`).

```elisp
(use-package org-modern-indent
  :hook
  (org-modern-mode . org-modern-indent-mode))
```
