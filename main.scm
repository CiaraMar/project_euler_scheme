(load "utils.scm")
(define problem_number
  (last-element (command-line)))
(load (string-append "problems/problem" problem_number ".scm"))
(display answer)
(newline)
