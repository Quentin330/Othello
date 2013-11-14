; 0 si vide
; 1 si blanc
; 2 si noir
; 3 si triche
; 6 si la famille

; cree le tableau du debut de la partie
(defun init-tab()
  (make-array '(8 8) :initial-contents
	      '((0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 1 2 0 0 0)
		(0 0 0 2 1 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0))))

; affiche le tableau
(defun show-tab(tab)
  (dotimes (i 8)
    (dotimes (j 8)
      (format t "~2D"
	      (aref tab j i)))
    (format t "~%"))
  (format t "~%"))

; boolean
(defun coup-valide(tab joueur x y)
  (if (case-vide tab x y)
      (if (or (prise-possible-droite tab joueur x y) (prise-possible-bas-droite tab joueur x y) (prise-possible-bas tab joueur x y) (prise-possible-bas-gauche tab joueur x y) (prise-possible-gauche tab joueur x y) (prise-possible-haut-gauche tab joueur x y) (prise-possible-haut tab joueur x y) (prise-possible-haut-droite tab joueur x y))
	  T
	  NIL)
      NIL))

(defun adversaire (joueur)
  (if (= joueur 1)
      2
      1))

(defun case-vide (tab x y)
  (if (or (zerop y) (zerop x) (< x 8) (< y 8)) ;; si on est dans le tableau
      (if (= 0 (aref tab x y)) ;; si la case est vide
	  T
	  NIL)
      NIL))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PRISES POSSIBLES !!
;;;

(defun prise-possible-droite (tab joueur x y)
      (if (< x 6)
	  (if (= (adversaire joueur) (aref tab (+ x 1) y))
	      (do ((i (+ 1 x) (+ 1 i)))((= 7 i))
		(if (= (aref tab i y) joueur)
		    (return T)
		    (if (case-vide tab i y)
			NIL)))
	      NIL)
	  NIL))


(defun prise-possible-gauche (tab joueur x y)
      (if (> x 1)
	  (if (= (adversaire joueur) (aref tab (- x 1) y))
	      (do ((i (- x 1) (- i 1)))((= 0 i))
		(if (= (aref tab i y) joueur)
		    (return T)
		    (if (case-vide tab i y)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-haut (tab joueur x y)
      (if (> y 1)
	  (if (= (adversaire joueur) (aref tab x (- y 1)))
	      (do ((i (- y 1) (- i 1)))((= 0 i))
		(if (= (aref tab x i) joueur)
		    (return T)
		    (if (case-vide tab x i)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-bas (tab joueur x y)
      (if (< y 6)
	  (if (= (adversaire joueur) (aref tab x (+ y 1)))
	      (do ((i (+ 1 y) (+ i 1)))((= 7 i))
		(if (= (aref tab x i) joueur)
		    (return T)
		    (if (case-vide tab x i)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-bas-droite (tab joueur x y)
      (if (and (< y 6) (< x 6))
	  (if (= (adversaire joueur) (aref tab (+ x 1) (+ y 1)))
	      (do ((i (+ 1 x) (+ i 1))(j (+ 1 y) (+ j 1)))
		  ((or (= 7 i) (= 7 j)))
		(if (= (aref tab i j) joueur)
		    (return T)
		    (if (case-vide tab i j)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-bas-gauche (tab joueur x y)
      (if (and (< y 6) (> x 1))
	  (if (= (adversaire joueur) (aref tab (- x 1) (+ y 1)))
	      (do ((i (- x 1) (- i 1))(j (+ y 1) (+ j 1)))
		  ((or (= 0 i) (= 7 j)))
		(if (= (aref tab i j) joueur)
		    (return T)
		    (if (case-vide tab i j)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-haut-gauche (tab joueur x y)
      (if (and (> y 1) (> x 1))
	  (if (= (adversaire joueur) (aref tab (- x 1) (- y 1)))
	      (do ((i (- x 1) (- i 1))(j (- y 1) (- j 1)))
		  ((or (= 0 i) (= 0 j)))
		(if (= (aref tab i j) joueur)
		    (return T)
		    (if (case-vide tab i j)
			NIL)))
	      NIL)
	  NIL))

(defun prise-possible-haut-droite (tab joueur x y)
      (if (and (> y 1) (< x 6))
	  (if (= (adversaire joueur) (aref tab (+ x 1) (- y 1)))
	      (do ((i (+ x 1) (+ i 1))(j (- y 1) (- j 1)))
		  ((or (= 7 i) (= 0 j)))
		(if (= (aref tab i j) joueur)
		    (return T)
		    (if (case-vide tab i j)
			NIL)))
	      NIL)
	  NIL))

                                                        ;;;
                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jouer-coup (tab joueur x y)
  (assert (coup-valide tab joueur x y))
  (setf (aref tab x y) joueur)
  (prise tab joueur x y)
  (show-tab tab)
)

(defun prise (tab joueur x y)
;droite
  (if (prise-possible-droite tab joueur x y)
      (do ((i (+ 1 x) (+ 1 i)))((> i 6))
	(if (= (aref tab i y) (adversaire joueur))
	    (setf (aref tab i y) joueur)
	    (setf i 7))))
;gauche
  (if (prise-possible-gauche tab joueur x y)
      (do ((i (- x 1) (- i 1)))((< i 1))
	(if (= (aref tab i y) (adversaire joueur))
	    (setf (aref tab i y) joueur)
	    (setf i 0)
	    )))
;haut
 (if (prise-possible-haut tab joueur x y)
     (do ((i (- y 1) (- i 1)))((< i 1))
       (if (= (aref tab x i) joueur)
	   (setf i 0)
	   (setf (aref tab x i) joueur))))
;bas
  (if (prise-possible-bas tab joueur x y)
      (do ((i (+ 1 y) (+ i 1)))((> i 6))
	(if (= (aref tab x i) joueur)
	   (setf i 7)
	   (setf (aref tab x i) joueur)))) 
;bas droite
  (if (prise-possible-bas-droite tab joueur x y)
      (do ((i (+ 1 x) (+ i 1))(j (+ 1 y) (+ j 1)))
	  ((or (> i 6) (> j 6)))
	(if (= (aref tab i j) joueur)
	    (setf i 7)
	    (setf (aref tab i j) joueur)))) 
;bas gauche
  (if (prise-possible-bas-gauche tab joueur x y)
      (do ((i (- x 1) (- i 1))(j (+ y 1) (+ j 1)))
	  ((or (< i 1) (> j 6)))
	(if (= (aref tab i j) joueur)
	    (setf i 0)
	    (setf (aref tab i j) joueur)))) 
;haut gauche
  (if (prise-possible-haut-gauche tab joueur x y)
      (do ((i (- x 1) (- i 1))(j (- y 1) (- j 1)))
	  ((or (< i 1) (< j 1)))
	(if (= (aref tab i j) joueur)
	    (setf i 0)
	    (setf (aref tab i j) joueur)))) 
;haut droite
  (if (prise-possible-haut-droite tab joueur x y)
      (do ((i (+ x 1) (+ i 1))(j (- y 1) (- j 1)))
	  ((or (> i 6) (< j 1)))
	(if (= (aref tab i j) joueur)
	    (setf i 7)
	    (setf (aref tab i j) joueur)))))


; jeu
(defun othello()
  (let ((tab (init-tab)))
    (setf (aref tab 5 4) 2)
    (setf (aref tab 4 4) 2)
    (setf (aref tab 3 4) 1)
    (setf (aref tab 3 5) 1)
    (jouer-coup tab 1 6 4)))
