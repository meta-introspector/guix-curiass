;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((fill-column . 78)
     (tab-width   .  8)

     ;; For use with 'bug-reference-prog-mode'.
     (bug-reference-url-format . "http://bugs.gnu.org/%s")
     (bug-reference-bug-regexp
      . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))
 (scheme-mode
  .
  ((indent-tabs-mode . nil)
   (eval . (put 'call-with-time 'scheme-indent-function 1))
   (eval . (put 'test-error 'scheme-indent-function 1))
   (eval . (put 'make-parameter 'scheme-indent-function 1))
   (eval . (put 'with-database 'scheme-indent-function 1)))))
