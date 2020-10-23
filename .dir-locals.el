;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((fennel-mode . ((eval . (put 'test 'fennel-indent-function 'defun))
                 (eval . (put 'when-some 'fennel-indent-function 1))
                 (eval . (put 'if-some 'fennel-indent-function 1))
                 (eval . (put 'when-let 'fennel-indent-function 1))
                 (eval . (put 'if-let 'fennel-indent-function 1))
                 (eval . (put 'fn* 'fennel-indent-function 'defun)))))
