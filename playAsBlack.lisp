(load "reversi-package.lisp")
(use-package 'reversi-package)
(load "reversync.lisp")

(reversi #'human (alpha-beta-searcher 2 #'eval-fn))



