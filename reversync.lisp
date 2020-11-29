;;;;ReverSync Heuristic;;;;
;;;;Author. Miguel Gonzalez;;;;

;;; Set the genome obtained with the genetic trainer
(defun eval-fn (player board) (eval-fnT player board '(570.28687 843.74332 688.3618 703.0719 248.01865 611.9509 430.4137
                                         660.2291 778.94165 618.968 455.427 286.3677 452.48486 234.13383
                                         507.51825 607.4481 93.699646 844.61414 65.71706 844.86127 369.70847)))

;;; Heuristic ;;;
(defun eval-fnT (player board genome)
  (let ((mobility (mobility player board))
        (anti-mobility (* -1 (mobility (opponent player) board)))
        (diff (count-difference player board))
	(positional (positional-adv2 player board))
        (tokens (count-tokens player board))
        (won (who-won player board))
        (potential-mobility (potential-mobility2 player board))
        (edge-st (no-puede-girar-imp player board))
        (unst (unstable player board))
        (diff-scalar1 (elt genome 0))
        (mobility-scalar1 (elt genome 1))
        (positional-scalar1 (elt genome 2))
        (edge-stable-scalar1 (elt genome 3))
        (pot-mobility-scalar1 (elt genome 4))
        (anti-mobility-scalar1 (elt genome 5))
        (unstable-scalar1 (elt genome 6))
        (diff-scalar2 (elt genome 7))
        (mobility-scalar2 (elt genome 8))
        (positional-scalar2 (elt genome 9))
        (edge-stable-scalar2 (elt genome 10))
        (pot-mobility-scalar2 (elt genome 11))
        (anti-mobility-scalar2 (elt genome 12))
        (unstable-scalar2 (elt genome 13))
        (diff-scalar3 (elt genome 14))
        (mobility-scalar3 (elt genome 15))
        (positional-scalar3 (elt genome 16))
        (edge-stable-scalar3 (elt genome 17))
        (pot-mobility-scalar3 (elt genome 18))
        (anti-mobility-scalar3 (elt genome 19))
        (unstable-scalar3 (elt genome 20)))
    
    (cond ((< tokens 21) (+ (* (weight_diffT tokens diff-scalar1) diff) (* mobility-scalar1 mobility) (* positional-scalar1 positional)
                            (* edge-stable-scalar1 edge-st) (* pot-mobility-scalar1 potential-mobility)
                            (* anti-mobility-scalar1 anti-mobility) (* unstable-scalar1 unst) (* (who_won_scalar) won)))
          ((< tokens 41) (+ (* (weight_diffT tokens diff-scalar2) diff) (* mobility-scalar2 mobility) (* positional-scalar2 positional)
                            (* edge-stable-scalar2 edge-st) (* pot-mobility-scalar2 potential-mobility)
                            (* anti-mobility-scalar2 anti-mobility) (* unstable-scalar2 unst) (* (who_won_scalar) won)))
          ((< tokens 64) (+ (* (weight_diffT tokens diff-scalar3) diff) (* mobility-scalar3 mobility) (* positional-scalar3 positional)
                            (* edge-stable-scalar3 edge-st) (* pot-mobility-scalar3 potential-mobility)
                            (* anti-mobility-scalar3 anti-mobility) (* unstable-scalar3 unst) (* (who_won_scalar) won)))
          (T (* (who_won_scalar) won)))))

;;; Helper functions

(defun weight_diffT (tokens ds)
  "Weight applied to token difference, changes over time"
  (* ds (atan (- tokens 32))))
(defun who_won_scalar NIL 1000000000) ;Tiene que ser muy alto

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

(defun count-tokens (player board)
  "Count player's pieces plus opponent's pieces."
  (let ((brd (get-board board)))
    (+ (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))
	   
(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

(defun replace-token (token player) 
  "Helper function that replaces token 1 (black) or 2 (white) by 1 on player color and -1 on opponent color"
  (cond ((= token 0) 0) ((= token 1) (if (= player white) -1 1)) ((= token 2) (if (= player white) 1 -1))))

(defun positional-adv-aux (player board)
  "Helper function that converts the board to a list of lists, each list being a row, each element
   being a 1 if player's token is there, a -1 if it's not, a 0 if there's no token"
  (let ((boardl (get-board board)))
    (mapcar #'(lambda (x)               ;For each row
                (mapcar #'(lambda (y) (replace-token y player))
                        x)) ;Apply mapcar to replace the element
            boardl)))

;Positional static scores for each tile, as established in: play-othello.appspot.com/files/Othello.pdf
(defun positional-scores NIL 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 -10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 -10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))

(defun positional-adv (player board)
  "Outputs positional advantage"
  (let ((weights (positional-adv-aux player board))
        (values (positional-scores)))
    (reduce #'+ (mapcar #'(lambda (x y)         ;Por cada fila en weigths y values
                (reduce #'+ (mapcar #'(lambda (z w) (* z w)) x y))) ;Se hace el producto escalar de las filas.
                        weights values))))

(defun positional-adv2 (player board)
  "Diff of positions"
  (- (positional-adv player board)
     (positional-adv (opponent player) board)))

(defun who-won (player board)
  "Outputs positive value in case position is player's win, negative if its player loss, 0 if game didnt end or tied"
  (if (every #'null (list (any-legal-move? player board) (any-legal-move? (opponent player) board))) ;Si nadie tiene jugadas legales
      (signum (count-difference player board))
      0))

(defun player-in-adjacent? (square player board)
  "Outputs whether player occupies any of the adjacent squares"
  (some #'(lambda (x) (eql (bref board x) player)) (neighbors square)))

(defun potential-mobility (player board)
  "Outputs the number of blank squares adjacent to opponent squares"
  (let ((counter 0))
    (dolist (square all-squares) ;Iterar por todas las casillas
      (when (eql (bref board square) empty) ;Si es una casilla vacia
        (when (player-in-adjacent? square (opponent player) board) (incf counter)))) ;Si el oponente esta adyacente
    counter))

(defun potential-mobility2 (player board)
  "Diff of potential mobilities"
  (- (potential-mobility player board)
     (potential-mobility (opponent player) board)))

(defun is-edge-stable-dir (square player board dir)
  "Whether along given direction there are only same-colored-pieces"
  (when (or (eql (bref board square) player) (eql (bref board square) outer))  ;La casilla debe ser de nuestro color o fuera.
    (or (eql (bref board square) outer) ;Si estamos fuera hemos acabado
      (is-edge-stable-dir (+ square dir) player board dir)))) ;llamada recursiva, moviendonos 

(defun is-unstable-dir1 (square player board dir)
  "Whether along given direction there are only same-colored-pieces but an opposite-colored eventually"
  (when (or (eql (bref board square) player) (eql (bref board square) (opponent player)))  ;La casilla debe ser de nuestro color o rival
    (or (eql (bref board square) (opponent player)) ;Si es del oponente hemos acabado.
        (is-unstable-dir1 (+ square dir) player board dir)))) ;llamada recursiva, moviendonos 

(defun is-unstable-dir2 (square player board dir)
  "Whether along given direction there are only same-colored-pieces but an empty space eventually"
  (when (or (eql (bref board square) player) (eql (bref board square) empty))  ;La casilla debe ser de nuestro color o rival
    (or (eql (bref board square) empty) ;Si es del oponente hemos acabado.
        (is-unstable-dir2 (+ square dir) player board dir)))) ;llamada recursiva, moviendonos

(defun is-unstable (square player board)
  "Whether along any direction there could be a flip. For example, if we had BW-, and we request the White piece, it would be 1 since it could be flipped."
  (reduce #'+ (mapcar #'(lambda (x) (if (and (eql (bref board square) player) ;Tiene que ser del jugador solicitante
                                             (is-unstable-dir1 square player board x) ;Tiene que haber en una direccion una opuesta
                                             (is-unstable-dir2 square player board (* -1 x))) ;En la otra direccion tiene que haber un blanco.
                                        1 0))
                      '(-11 -10 -9 -1 1 9 10 11)))) ;La lista esa son las posibles direcciones.

(defun unstable (player board)
  "Outputs unstability difference"
  (- (reduce #'+ (mapcar #'(lambda (x) (is-unstable x (opponent player) board)) all-squares))
     (reduce #'+ (mapcar #'(lambda (x) (is-unstable x player board)) all-squares))))

(defun is-stable (player board square)
  "Whether this piece cant ever be flipped. It's either because EDGE-SAME-SAME-...-PIECE-..., or
   because ENEMY-SAME....SAME-PIECE-SAME....-SAME-ENEMY."
  (if(notany #'null (mapcar #'(lambda(x)(or (is-edge-stable-dir square player board x)
                                            (is-edge-stable-dir square player board (* -1 x))
                                            (when (eql player (bref board square))(and (is-unstable-dir1 square player board x)
                                                 (is-unstable-dir1 square player board (* -1 x))))))
                            '(11 10 9 1)))
     1
     0))

(defun no-puede-girar-imp(player board)
  "Counts stable directions"
  (reduce #'+ (mapcar #'(lambda(x)(is-stable player board x)) all-squares))) 
                                        ;Suma todos los valores por cada casilla que indica 1 si se van a poder girar fichas, 0 si no