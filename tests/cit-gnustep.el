;;;; Test EDE GNUstep Project
;(require 'ede-pmake "../ede/ede-pmake.el" t)
(require 'ede-gnustep (concat cit-src-dir "../contrib/ede-gnustep.el") t)
;(defvar cit-ede-step-dir (concat cedet-integ-target "/edegnustep"))
(require 'edebug)
(defun cit-ede-step-test ()
  "Test EDE GNUstep-Make Project"
;  (edebug)
;  (message "I'm in %s" (pwd))
  (ede-new "GNUstep-Make" "EDE GNUstep Integration Test")
  (ede-new-target "test0" "tool")
  (find-file "main.c")
  (insert
   "#include <stdio.h>\nint main (){ printf(\"Hello CEDET!\"); }\n")
  (save-buffer)
  (ede-add-file "test0")
  (ede-commit-project (ede-current-project))
  (find-file "ProjStep.ede")
  (ede-proj-regenerate)
  (if (getenv "GNUSTEP_MAKEFILES")
      (ede-compile-project)
    (progn
      (message "I noticed that you didn't load `GNUstep.sh' for the GNUstep-Make Environment ...")
      (message "I'll compile this simple examle via gcc ... but, use gnustep ... is better ;)")
      (compile "gcc -o Prog main.c")))
)

(provide 'cit-gnustep)
