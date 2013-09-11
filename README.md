# tagshow - tags utility


Requirements
============

- [grizzl][1]

- [dash][2]


Configure
=========

Add such lines to your ~/.emacs:

    ```lisp
    (add-to-list 'load-path "~/R/ep/tagshow")
    (require 'tagshow)
    (tagshow-mode 1)
    ```

[1]: http://github.com/d11wtq/grizzl
[2]: https://github.com/magnars/dash.el