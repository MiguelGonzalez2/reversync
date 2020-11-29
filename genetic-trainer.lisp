;;;;;;;;;;;Genetic trainer for evaluation functions. Author: Miguel Gonzalez;;;;;;;;;;;;;

(load "reversi-package.lisp")
(use-package 'reversi-package)
(load "reversync.lisp") ;Load here a lisp file that has the heuristic function called eval-fnT which accepts the player, the board, and the list of weights.

;;;;;;;;;;;;;;;;;;;;;;;;;;;Parameters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *inf* 0) ;Minimum weight value
(defparameter *sup* 1000)  ;Maximum weight value
(defparameter *num-pesos* 21)  ;Number of weights to fit
(defparameter *mut-prob* 0.05)  ;Probability of mutation
(defparameter *num-iters* 5)  ;Number of iterations

;;;;;;;;;;;;;;;;;;;;;;;;;;;Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rand-float (inf sup)
  "Genera un flotante aleatorio entre inf y sup"
  (+ inf (* (- sup inf) (random 1.0))))

(defun rand-float-list (inf sup N)
  "Genera una lista de N flotantes aleatorios entre inf y sup"
  (mapcar #'(lambda (x) (rand-float x sup)) (make-list N :initial-element inf)))

(defun rand-float-list-of-lists (inf sup N M)
  "Genera una lista de M listas de N flotantes aleatorios entre inf y sup"
  (loop for i from 1 to M collect (rand-float-list inf sup N)))

(defun breed-gene (g1 g2 mp inf sup)
  "Esta funcion devuelve un gen resultantes de cruzar g1 y g2.
   mp es la probabilidad de mutacion de g1 y g2. Inf y Sup son los extremos del problema."
  (if (< (random 1.0) mp) (rand-float inf sup) (rand-float g1 g2)))

(defun breed-genome (g1 g2 mp inf sup)
  "Funcion que cruza 2 genomas"
  (mapcar #'(lambda (x y) (breed-gene x y mp inf sup)) g1 g2))

(defun genomes-breeder (genomes mp inf sup)
  "Dado una lista con 2K genomas, devuelve otro con 2K genomas resultado de cruzar pares consecutivos de genomas
  mp: Probabilidad de mutacion. inf,sup: Limites inferior y superior para los valores."
  (let ((new-genomes (make-list (length genomes) :initial-element 0)))
  (do ((i 0 (+ 2 i))) ((= i (length genomes)))
     (setf (elt new-genomes i) (breed-genome (elt genomes i) (elt genomes (+ i 1)) mp inf sup))
     (setf (elt new-genomes (+ i 1)) (breed-genome (elt genomes i) (elt genomes (+ i 1)) mp inf sup)))
    new-genomes))

(defun biased-generator (values weights)
  "Generador aleatorio con pesos ENTEROS. Por ejemplo si values=(a b) y weights = (1 2), 1/3 de las veces dara A.
   Funcion de Joshua Taylor (Stack overflow)" 
  (multiple-value-bind (total values)
      (loop for v in values
            for w in weights
            nconc (make-list w :initial-element v) into vs
            sum w into total
            finally (return (values total (coerce vs 'vector))))
    (lambda ()
      (aref values (random total)))))

(defun biased-list (values weights N)
  "Genera una lista de N elementos tomados de los valores values con pesos (probabilidades de eleccion) weights)"
  (let ((gen (biased-generator values weights)))
    (loop for i from 1 to N collect (funcall gen))))

(defun genetic-algorithm (start K new-gen mp inf sup)
  "Algoritmo genetico. Start: Lista con genomas iniciales. K: Numero de iteraciones. new-gen: Funcion que dada una lista de
   genomas, devuelve una lista de pesos/fitnesses ENTEROS de cada genoma. SALIDA: Listado de los genomas de la T-esima iteracion."
  (if (= K 0) start ;Si no quedan iteraciones devolvemos los genomas.
      (let ((fitnesses (funcall new-gen start)))
        (genetic-algorithm (genomes-breeder (biased-list start fitnesses (length start)) mp inf sup) 
                         (- K 1)
                         new-gen mp inf sup))))
						 
(defun avg (list)
  (/ (reduce #'+ list) (length list)))

(defun sigmoidify (x)
  (+ 1 (floor (/ 1000 (+ 1 (exp (* (/ 1 18) (+ 100 (* -1 x)))))))))

(defun sigmoidify2 (x c)
  (+ 1 (floor (/ 1000 (+ 1 (exp (* (/ 1 18) (+ c (* -1 x)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Training;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun round-robin-scores (strategies n-pairs &optional
                                         (n-random 10))
  "Play a tournament among the strategies.
  N-PAIRS = games each strategy plays as each color against
  each opponent.  So with N strategies, a total of
  N*(N-1)*N-PAIRS games are played. OUTPUTS SCORE VECTOR"
  (let* ((N (length strategies))
         (totals (make-list N :initial-element 0)))
    ;; Play the games
    (dotimes (i 1)
      (loop for j from (+ i 1) to (- N 1) do
            (let* ((wins (random-reversi-series ; reversi-series
                          (elt strategies i)
                          (elt strategies j)
                          n-pairs n-random))
                   (losses (- (* 2 n-pairs) wins)))
              (incf (elt totals i) wins)
              (incf (elt totals j) losses))))
    ;; ReturnTheScores
    (mapcar #'(lambda (x) (* 2 x)) totals)))

(defparameter *test-gens* (rand-float-list-of-lists *inf* *sup* *num-pesos* 10))
(defparameter *train-gens* (rand-float-list-of-lists *inf* *sup* *num-pesos* 10))

(defun new-gen-reversi (genomes)
  (let ((fitnesses (mapcar #'(lambda (Z) (first (round-robin-scores
                                                 (list (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y Z)))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 0))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 1))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 2))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 3))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 4))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 5))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 6))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 7))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 8))))
                                                       (alpha-beta-searcher 2 #'(lambda (x y) (eval-fnT x y (elt *test-gens* 9)))))
                                                 5
                                                 5)))
                           genomes)))
    (print fitnesses)
    (mapcar #'(lambda (x) (sigmoidify2 x (avg fitnesses))) fitnesses)))

(defun lets-go NIL (let* ((results (genetic-algorithm *train-gens*
                   *num-iters* #'new-gen-reversi *mut-prob* *inf* *sup*))
				   (fitnesses (new-gen-reversi results)))
				   (elt results (position (reduce #'max fitnesses) fitnesses))))
				   
;; Call the trainer
(lets-go)
