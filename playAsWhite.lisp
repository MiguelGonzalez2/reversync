(load "reversi-package.lisp")
(use-package 'reversi-package)
(load "reversync.lisp")

(reversi (alpha-beta-searcher 2 #'eval-fn) #'human )



