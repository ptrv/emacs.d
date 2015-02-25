(defconst pcmpl-go-commands
  '("build" "clean" "doc" "env" "fix" "fmt" "get" "install"
    "list" "run" "test" "tool" "version" "vet" )
  "List of `go' commands")

(defun pcomplete/go ()
  "Completion for `go'"
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-go-commands)
  (cond
   ((pcomplete-match (regexp-opt '("build" "clean" "doc" "fix"
                                   "run" "vet")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ))

(provide 'pcmpl-go)
