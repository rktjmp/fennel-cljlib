;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((fennel-mode . ((eval . (font-lock-add-keywords
                          'fennel-mode
                          `((,(rx word-start
                                  (group (or "fn*"
                                             "try"
                                             "catch"
                                             "finally"
                                             "if-let"
                                             "if-some"
                                             "when-let"
                                             "when-some"
                                             "empty"
                                             "into"
                                             "when-meta"
                                             "with-meta"
                                             "meta"
                                             "meta"
                                             "def"
                                             "defn"
                                             "defmulti"
                                             "defmethod"
                                             "defonce"
                                             "deftest"
                                             "testing"
                                             "assert-eq"
                                             "assert-ne"
                                             "assert-is"
                                             "assert-not"
                                             "loop"))
                                  word-end)
                             1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords
                          'fennel-mode
                          `((,(rx (syntax open-parenthesis)
                                  (or "fn*" "defn" "defmulti") (1+ space)
                                  (group (1+ (or (syntax word) (syntax symbol) "-" "_"))))
                             1 'font-lock-function-name-face))))
                 (eval . (put 'when-meta 'fennel-indent-function 'defun))
                 (eval . (put 'defmethod 'fennel-indent-function 3))
                 (eval . (put 'defmulti 'bfennel-indent-function 'defun))
                 (eval . (put 'deftest 'fennel-indent-function 'defun))
                 (eval . (put 'testing 'fennel-indent-function 'defun))
                 (eval . (put 'when-some 'fennel-indent-function 1))
                 (eval . (put 'if-some 'fennel-indent-function 1))
                 (eval . (put 'when-let 'fennel-indent-function 1))
                 (eval . (put 'if-let 'fennel-indent-function 1))
                 (eval . (put 'loop 'fennel-indent-function 1))
                 (eval . (put 'fn* 'fennel-indent-function 'defun))
                 (eval . (put 'fn* 'fennel-doc-string-elt 2))
                 (eval . (put 'defn 'fennel-indent-function 'defun))
                 (eval . (put 'defn 'fennel-doc-string-elt 2))
                 (eval . (put 'defmulti 'fennel-doc-string-elt 2))
                 (eval . (put 'try 'fennel-indent-function 0))
                 (eval . (put 'catch 'fennel-indent-function 1))
                 (eval . (put 'finally 'fennel-indent-function 0)))))
