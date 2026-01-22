((emacs-lisp-mode
  . ((eval . (add-to-list 'load-path
                          (expand-file-name (locate-dominating-file default-directory ".dir-locals.el")))))))
