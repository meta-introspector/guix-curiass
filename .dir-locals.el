;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (fill-column . 78)
  (tab-width . 8)
  (bug-reference-url-format . "http://bugs.gnu.org/%s")
  (bug-reference-bug-regexp
   . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>"))
 (scheme-mode
  (indent-tabs-mode)
  (eval put 'call-with-time 'scheme-indent-function 1)
  (eval put 'test-error 'scheme-indent-function 1)
  (eval put 'make-parameter 'scheme-indent-function 1)
  (eval put 'with-database 'scheme-indent-function 1))
 (texinfo-mode
  (indent-tabs-mode)
  (fill-column . 72)
  (ispell-local-dictionary . "american")))
