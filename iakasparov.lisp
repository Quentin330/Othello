(defparameter valeurvictoire 1000)
(defparameter profondeur 4)

;strategies
(defparameter alladv 1)
(defparameter bestadv 0)

;jouer-coup
(defparameter avectest 1)
(defparameter sanstest 0)

(defun waitenter()
  (let ((char #\Space))
    (do ()((char-equal char #\Newline))
      (setf char (read-char)))))
  

(defun visu-strats ()
  (let ((tab (init-tab)) (partiefinie NIL))
    (progn
      (show-tab tab)
      (waitenter)
      (do ()(partiefinie)
	(format t "L'ia all-adv (joueur 1) joue en : ")
	(jouer-coup-ia tab 1 profondeur :strat 1)
	(waitenter)
	(show-tab tab)
	(waitenter)
	(format t "L'ia best-adv (joueur 2) joue en : ")
	(jouer-coup-ia tab 2 profondeur :strat 0)
	(waitenter)
	(show-tab tab)
	(waitenter)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 2)
		  (format t "L'IA best-adv (joueur 2) a gagné ~%")
		  (if (victoire tab 1)
		      (format t "L'IA all-adv (joueur 1) a gagné ~%")
		      (format t "Egalité ~%"))))))
      (setf tab (init-tab))
      (setf partiefinie NIL)
      (waitenter)
      (show-tab tab)
      (waitenter)
      (do ()(partiefinie)
	(format t "L'ia best-adv (joueur 1) joue en : ")
	(jouer-coup-ia tab 1 profondeur :strat 0)
	(waitenter)
	(show-tab tab)
	(waitenter)
	(format t "L'ia all-adv (joueur 2) joue en : ")
	(jouer-coup-ia tab 2 profondeur :strat 1)
	(waitenter)
	(show-tab tab)
	(waitenter)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 2)
		  (format t "L'IA all-adv (joueur 2) a gagné ~%")
		  (if (victoire tab 1)
		      (format t "L'IA best-adv (joueur 1) a gagné ~%")
		      (format t "Egalité ~%"))))))))
  NIL)
    
      


(defun main-affichage (first)
  (let ((tab (init-tab)) (partiefinie NIL))
    (show-tab tab)
    (if first
	(do ((i 0 (+ i 1)))(partiefinie)
	  (format t "L'ia joue en : ")
	  (jouer-coup-ia tab 1 profondeur)
	  (show-tab tab)
	  (format t "à vous de jouer (joueur 2) : ")
	  (lire-coup tab 2 :affichage T)
	  (show-tab tab)
	  (if (fin-partie tab)
	      (progn
		(setf partiefinie T)
		(if (victoire tab 2)
		    (format t "Le joueur a gagné ~%")
		    (if (victoire tab 1)
			(format t "L'IA a gagné ~%")
			(format t "Egalité ~%"))))))
	(do ((i 0 (+ i 1)))(partiefinie)
	  (format t "à vous de jouer (joueur 1) : ")
	  (lire-coup tab 1 :affichage T)
	  (show-tab tab)
	  (format t "L'ia joue en : ")
	  (jouer-coup-ia tab 2 profondeur)
	  (show-tab tab)
	  (if (fin-partie tab)
	      (progn
		(setf partiefinie T)
		(if (victoire tab 1)
		    (format t "Le joueur a gagné ~%")
		    (if (victoire tab 2)
			(format t "L'IA a gagné ~%")
			(format t "Egalité ~%")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;la fonction jouer-coup-sans-test est plus rapide et elle permet de supprimer des tests en amont quelques fois, nous allons donc utiliser cette fonction
(defun test-vitesse-jouer-coup()
  (let ((tab1 (init-tab))(tab2 (init-tab))(begintime 0) (endtime 0) (time-avec-test)(time-sans-test))
    (setf begintime (get-internal-real-time))
    (do ((i 0 (+ i 1)))((= 1000 i))
      (setf tab1 (init-tab))
      (jouer-coup-sans-test tab1 1 2 4)
      (jouer-coup-sans-test tab1 2 2 3)
      (jouer-coup-sans-test tab1 1 1 2)
      (jouer-coup-sans-test tab1 2 4 5)
      (jouer-coup-sans-test tab1 1 5 6)
      (jouer-coup-sans-test tab1 2 1 4)
      (jouer-coup-sans-test tab1 1 4 2)
      (jouer-coup-sans-test tab1 2 5 5)
      (jouer-coup-sans-test tab1 1 2 5)
      (jouer-coup-sans-test tab1 2 3 5)
      (jouer-coup-sans-test tab1 1 6 5))
    (setf endtime (get-internal-real-time))
    (setf time-sans-test (- endtime begintime))
    (format t "Sans tests : ~D millisecondes; " time-sans-test)
    (setf begintime (get-internal-real-time))
    (do ((i 0 (+ i 1)))((= 1000 i))
      (setf tab2 (init-tab))
      (jouer-coup tab2 1 2 4)
      (jouer-coup tab2 2 2 3)
      (jouer-coup tab2 1 1 2)
      (jouer-coup tab2 2 4 5)
      (jouer-coup tab2 1 5 6)
      (jouer-coup tab2 2 1 4)
      (jouer-coup tab2 1 4 2)
      (jouer-coup tab2 2 5 5)
      (jouer-coup tab2 1 2 5)
      (jouer-coup tab2 2 3 5)
      (jouer-coup tab2 1 6 5))
    (setf endtime (get-internal-real-time))
    (setf time-avec-test (- endtime begintime))
    (format t "Avec tests : ~D millisecondes : " time-avec-test)
    (if (> time-avec-test time-sans-test)
	(format t "Coup plus lent avec test~%")
	(format t "Coup plus rapide avec test/!\~%"))))
    

;fin partie de test a supprimer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    


(defun test-temps-de-reponse (nbrtests strat)
  (let ((tab (init-tab)) (partiefinie NIL) (begintime 0) (endtime 0) (nbrovertime 0) (nbrcoups 0))
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(setf begintime (get-internal-real-time))
	(jouer-coup-ia tab 1 profondeur :ecrirecoup NIL :strat strat)
	(setf endtime (get-internal-real-time))
	(setf nbrcoups (+ 1 nbrcoups))
	(jouer-coup-random tab 2 :ecrirecoup NIL)
	(if (> (- endtime begintime) 999)
	    (progn
	      (format t "/!\ TEMPS DEPASSE /!\ (~D millisecondes)~%" (- endtime begintime))
	      (setf nbrovertime (+ nbrovertime 1)))
	    (format t "Temps non dépassé (~D millisecondes)~%" (- endtime begintime)))
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(jouer-coup-random tab 1 :ecrirecoup NIL)
	(setf begintime (get-internal-real-time))
	(jouer-coup-ia tab 2 profondeur :ecrirecoup NIL :strat strat)
	(setf endtime (get-internal-real-time))
	(setf nbrcoups (+ 1 nbrcoups))
	(if (> (- endtime begintime) 999)
	    (progn
	      (format t "/!\ TEMPS DEPASSE /!\ (~D millisecondes)~%" (- endtime begintime))
	      (setf nbrovertime (+ nbrovertime 1)))
	    (format t "Temps non dépassé (~D millisecondes)~%" (- endtime begintime)))
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (format t "Sur ~D coups de l'IA dans ~D parties, le temps a été dépassé ~D fois~%" nbrcoups (* 2 nbrtests) nbrovertime)
    ))

(defun test-strategie (nbrtests strat)
  (let ((tab (init-tab)) (partiefinie NIL) (win 0) (win1 0) (win2 0))
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(format t "Partie ~D coup ~D~%" (+ 1 i) (+ 1 (* 2 j)))
	(jouer-coup-ia tab 1 profondeur :ecrirecoup NIL :strat strat)
	(format t "Partie ~D coup ~D~%" (+ 1 i) (+ 2 (* 2 j)))
	(jouer-coup-random tab 2 :ecrirecoup NIL)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 1)
		  (progn
		    (format t "VICTOIRE IA~%")
		    (setf win (+ win 1)))
		  (format t "DEFAITE IA~%")))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (setf win1 win)
    (setf win 0)
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(format t "Partie ~D coup ~D~%" (+ 1 (+ nbrtests i)) (+ 1 (* 2 j)))
	(jouer-coup-random tab 1 :ecrirecoup NIL)
	(format t "Partie ~D coup ~D~%" (+ 1 (+ nbrtests i)) (+ 2 (* 2 j)))
	(jouer-coup-ia tab 2 profondeur :ecrirecoup NIL :strat strat)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 2)
		  (progn
		    (format t "VICTOIRE IA~%")
		    (setf win (+ win 1)))
		 (format t "DEFAITE IA~%")))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (setf win2 win)
    (setf win 0)
    (format t "En commençant, l'ia gagne ~D% de ses parties (~D victoires sur ~D parties)~%" (/ win1 (/ nbrtests 100)) win1 nbrtests)
    (format t "En jouant en 2e, l'ia gagne ~D% de ses parties (~D victoires sur ~D parties)~%" (/ win2 (/ nbrtests 100)) win1 nbrtests)
    ))

(defun best-strategie (nbrtests)
       (let ((tab (init-tab)) (partiefinie NIL) (win 0) (win1 0) (win2 0))
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(format t "Partie ~D coup ~D~%" (+ 1 i) (+ 1 (* 2 j)))
	(jouer-coup-ia tab 1 profondeur :ecrirecoup NIL :strat 0)
	(format t "Partie ~D coup ~D~%" (+ 1 i) (+ 2 (* 2 j)))
	(jouer-coup-ia tab 2 profondeur :ecrirecoup NIL :strat 1)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 1)
		  (progn
		    (format t "VICTOIRE IA BEST ADV~%")
		    (setf win (+ win 1)))
		  (format t "VICTOIRE IA ALL ADV~%")))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (setf win1 win)
    (setf win 0)
    (do ((i 0 (+ i 1)))((= i nbrtests))
      (do ((j 0 (+ j 1)))(partiefinie)
	(format t "Partie ~D coup ~D~%" (+ 1 (+ nbrtests i)) (+ 1 (* 2 j)))
	(jouer-coup-ia tab 1 profondeur :ecrirecoup NIL :strat 1)
	(format t "Partie ~D coup ~D~%" (+ 1 (+ nbrtests i)) (+ 2 (* 2 j)))
	(jouer-coup-ia tab 2 profondeur :ecrirecoup NIL :strat 0)
	(if (fin-partie tab)
	    (progn
	      (setf partiefinie T)
	      (if (victoire tab 2)
		  (progn
		    (format t "VICTOIRE IA BEST ADV~%")
		    (setf win (+ win 1)))
		 (format t "VICTOIRE IA ALL ADV~%")))))
      (setf tab (init-tab))
      (setf partiefinie NIL))
    (setf win2 win)
    (setf win 0)
    (format t "En commençant, l'ia best adv gagne ~D% de ses parties (~D victoires sur ~D parties)~%" (/ win1 (/ nbrtests 100)) win1 nbrtests)
    (format t "En jouant en 2e, l'ia gagne best adv ~D% de ses parties (~D victoires sur ~D parties)~%" (/ win2 (/ nbrtests 100)) win1 nbrtests)
    )) 

(defun main-standalone (first)
  (let ((tab (init-tab)) (partiefinie NIL))
    (if first
	(do ((i 0 (+ i 1)))(partiefinie)
	  (jouer-coup-ia tab 1 profondeur)
	  (lire-coup tab 2)
	  (if (fin-partie tab)
		(setf partiefinie T)))
	(do ((i 0 (+ i 1)))(partiefinie)
	  (lire-coup tab 1)
	  (jouer-coup-ia tab 2 profondeur)
	  (if (fin-partie tab)
		(setf partiefinie T)
	      )))))


; affiche le tableau (a supprimer)
(defun show-tab(tab)
  (format t "~%   A B C D E F G H~%")
  (dotimes (i 8)
    (format t "~2D" (+ 1 i))
    (dotimes (j 8)
      (format t "~2D"
	      (aref tab j i)))
    (format t "~%"))
  (format t "~%"))


(defun init-tab()
  (make-array '(8 8) :initial-contents
	      '((0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 2 1 0 0 0)
		(0 0 0 1 2 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0))))

(defun coup-valide(tab joueur x y)
  (if (case-vide tab x y)
      (if (or (prise-possible-droite tab joueur x y)
	      (prise-possible-bas-droite tab joueur x y)
	      (prise-possible-bas tab joueur x y)
	      (prise-possible-bas-gauche tab joueur x y)
	      (prise-possible-gauche tab joueur x y)
	      (prise-possible-haut-gauche tab joueur x y)
	      (prise-possible-haut tab joueur x y)
	      (prise-possible-haut-droite tab joueur x y))
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
	  (do ((i (+ 1 x) (+ 1 i)))((or (= 8 i) (case-vide tab i y)))
	    (if (= (aref tab i y) joueur)
		(return T)))
	  NIL)
      NIL))


(defun prise-possible-gauche (tab joueur x y)
  (if (> x 1)
      (if (= (adversaire joueur) (aref tab (- x 1) y))
	  (do ((i (- x 1) (- i 1)))((or (= (- 0 1) i) (case-vide tab i y)))
	    (if (= (aref tab i y) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-haut (tab joueur x y)
  (if (> y 1)
      (if (= (adversaire joueur) (aref tab x (- y 1)))
	  (do ((i (- y 1) (- i 1)))((or (= (- 0 1) i) (case-vide tab x i)))
	    (if (= (aref tab x i) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-bas (tab joueur x y)
  (if (< y 6)
      (if (= (adversaire joueur) (aref tab x (+ y 1)))
	  (do ((i (+ 1 y) (+ i 1)))((or (= 8 i) (case-vide tab x i)))
	    (if (= (aref tab x i) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-bas-droite (tab joueur x y)
  (if (and (< y 6) (< x 6))
      (if (= (adversaire joueur) (aref tab (+ x 1) (+ y 1)))
	  (do ((i (+ 1 x) (+ i 1))(j (+ 1 y) (+ j 1)))
	      ((or (= 8 i) (= 8 j) (case-vide tab i j)))
	    (if (= (aref tab i j) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-bas-gauche (tab joueur x y)
  (if (and (< y 6) (> x 1))
      (if (= (adversaire joueur) (aref tab (- x 1) (+ y 1)))
	  (do ((i (- x 1) (- i 1))(j (+ y 1) (+ j 1)))
	      ((or (= (- 0 1) i) (= 8 j) (case-vide tab i j)))
	    (if (= (aref tab i j) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-haut-gauche (tab joueur x y)
  (if (and (> y 1) (> x 1))
      (if (= (adversaire joueur) (aref tab (- x 1) (- y 1)))
	  (do ((i (- x 1) (- i 1))(j (- y 1) (- j 1)))
	      ((or (= (- 0 1) i) (= (- 0 1) j)(case-vide tab i j)))
	    (if (= (aref tab i j) joueur)
		(return T)))
	  NIL)
      NIL))

(defun prise-possible-haut-droite (tab joueur x y)
  (if (and (> y 1) (< x 6))
      (if (= (adversaire joueur) (aref tab (+ x 1) (- y 1)))
	  (do ((i (+ x 1) (+ i 1))(j (- y 1) (- j 1)))
	      ((or (= 8 i) (= (- 0 1) j) (case-vide tab i j)))
	    (if (= (aref tab i j) joueur)
		(return T)))
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


(defun jouer-coup-sans-test (tab joueur x y)
  (let ((changed NIL)(eaten NIL))
    (if (= 0 (aref tab x y))
	(progn
;droite
	  (if (< x 6)
	      (if (= (adversaire joueur) (aref tab (+ x 1) y))
		  (progn
		    (do ((i (+ x 1) (+ i 1)))((or (> i 7) (case-vide tab i y) eaten))
		      (if (= joueur (aref tab i y))
			  (progn
			    (do ((l (- i 1) (- l 1)))((= x l))
			      (setf (aref tab l y) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;gauche
	  (if (> x 1)
	      (if (= (adversaire joueur) (aref tab (- x 1) y))
		  (progn
		    (do ((i (- x 1) (- i 1)))((or (< i 0) (case-vide tab i y) eaten))
		      (if (= joueur (aref tab i y))
			  (progn
			    (do ((l (+ i 1) (+ l 1)))((= x l))
			      (setf (aref tab l y) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;haut
	  (if (> y 1)
	      (if (= (adversaire joueur) (aref tab x (- y 1)))
		  (progn
		    (do ((i (- y 1) (- i 1)))((or (< i 0) (case-vide tab x i) eaten))
		      (if (= joueur (aref tab x i))
			  (progn
			    (do ((h (+ i 1) (+ h 1)))((= y h))
			      (setf (aref tab x h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;bas
	  (if (< y 6)
	      (if (= (adversaire joueur) (aref tab x (+ y 1)))
		  (progn
		    (do ((i (+ y 1) (+ i 1)))((or (> i 7) (case-vide tab x i) eaten))
		      (if (= joueur (aref tab x i))
			  (progn
			    (do ((h (- i 1) (- h 1)))((= y h))
			      (setf (aref tab x h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;bas droite
	  (if (and (< x 6) (< y 6))
	      (if (= (adversaire joueur) (aref tab (+ x 1) (+ y 1)))
		  (progn
		    (do ((i (+ x 1) (+ i 1))(j (+ y 1) (+ j 1))) ((or (> i 7) (> j 7) (case-vide tab i j) eaten))
		      (if (= joueur (aref tab i j))
			  (progn
			    (do ((l (- i 1) (- l 1)) (h (- j 1) (- h 1))) ((or (= x l) (= y h)))
			      (setf (aref tab l h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;bas gauche
	  (if (and (> x 1) (< y 6))
	      (if (= (adversaire joueur) (aref tab (- x 1) (+ y 1)))
		  (progn
		    (do ((i (- x 1) (- i 1))(j (+ y 1) (+ j 1))) ((or (< i 0) (> j 7) (case-vide tab i j) eaten))
		      (if (= joueur (aref tab i j))
			  (progn
			    (do ((l (+ i 1) (+ l 1)) (h (- j 1) (- h 1))) ((or (= x l) (= y h)))
			      (setf (aref tab l h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;haut droite
	  (if (and (< x 6) (> y 1))
	      (if (= (adversaire joueur) (aref tab (+ x 1) (- y 1)))
		  (progn
		    (do ((i (+ x 1) (+ i 1))(j (- y 1) (- j 1))) ((or (> i 7) (< j 0) (case-vide tab i j) eaten))
		      (if (= joueur (aref tab i j))
			  (progn
			    (do ((l (- i 1) (- l 1)) (h (+ j 1) (+ h 1))) ((or (= x l) (= y h)))
			      (setf (aref tab l h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))
;haut gauche
	  (if (and (> x 1) (> y 1))
	      (if (= (adversaire joueur) (aref tab (- x 1) (- y 1)))
		  (progn
		    (do ((i (- x 1) (- i 1))(j (- y 1) (- j 1))) ((or (< i 0) (< j 0) (case-vide tab i j) eaten))
		      (if (= joueur (aref tab i j))
			  (progn
			    (do ((l (+ i 1) (+ l 1)) (h (+ j 1) (+ h 1))) ((or (= x l) (= y h)))
			      (setf (aref tab l h) joueur))
			    (setf eaten T)
			    (setf changed T))))
		    (setf eaten NIL))))

	  (if changed
	      (progn
		(setf (aref tab x y) joueur)
		T)
	      NIL))
	NIL)))


(defun fin-partie (tab)
  (and (ne-peut-pas-jouer tab 1) (ne-peut-pas-jouer tab 2)))

(defun ne-peut-pas-jouer (tab joueur)
  (let ((bool T))
    (do ((i 0 (+ i 1)))((= i 8))
      (do ((j 0 (+ j 1)))((or (= j 8) (not bool)))
	(if (coup-valide tab joueur i j)
	    (setf bool nil))))
    bool))

(defun isspace (char)
  (if (or (char-equal char #\Newline) (char-equal char #\Space) (char-equal char #\Tab) (char-equal char #\Backspace)  (char-equal char #\Return) (char-equal char #\Linefeed))
      T
      NIL))

(defun convert-abscisse ()
  (let ((abscisse (read-char)))
    (if (isspace abscisse)
	(convert-abscisse)
	(if (char-equal abscisse #\a)
	    0
	    (if (char-equal abscisse #\b)
		1
		(if (char-equal abscisse #\c)
		    2
		    (if (char-equal abscisse #\d)
			3
			(if (char-equal abscisse #\e)
			    4
			    (if (char-equal abscisse #\f)
				5
				(if (char-equal abscisse #\g)
				    6
				    (if (char-equal abscisse #\h)
					7
					8)))))))))))

(defun convert-ordonnee ()
  (let ((ordonnee (read-char)))
    (if (isspace ordonnee)
	(convert-ordonnee)
	(if (char-equal ordonnee #\1)
	    0
	    (if (char-equal ordonnee #\2)
		1
		(if (char-equal ordonnee #\3)
		    2
		    (if (char-equal ordonnee #\4)
			3
			(if (char-equal ordonnee #\5)
			    4
			    (if (char-equal ordonnee #\6)
				5
				(if (char-equal ordonnee #\7)
				    6
				    (if (char-equal ordonnee #\8)
					7
					8)))))))))))

;version aleatoire avec test
(defun jouer-coup-random-old (tab joueur &key (ecrirecoup T))
  (let ((x 8)(y 8))
    (if (not (ne-peut-pas-jouer tab joueur))
	(progn
	  (do ((i 0 (+ i 1)))((coup-valide tab joueur x y))
	    (setf x (random 8))
	    (setf y (random 8)))
	  (if ecrirecoup
	      (progn
		(char-abscisse x)
		(char-ordonnee y)))
	  (jouer-coup tab joueur x y)
	  )
	)
   ))

;version aleatoire sans test
(defun jouer-coup-random (tab joueur &key (ecrirecoup T))
  (let ((x 8)(y 8)(bool NIL))
    (if (not (ne-peut-pas-jouer tab joueur))
	(progn
	  (do ((i 0 (+ i 1)))(bool)
	    (setf x (random 8))
	    (setf y (random 8))
	    (setf bool (jouer-coup-sans-test tab joueur x y)))
	  (if ecrirecoup
	      (progn
		(char-abscisse x)
		(char-ordonnee y)))
	  )
	)
   ))


(defun lire-coup (tab joueur &key (affichage NIL))
  (let ((coupvalide NIL) (x 0) (y 0))
    (if (not (ne-peut-pas-jouer tab joueur))
	(do ()(coupvalide)
	  (setf x (convert-abscisse))
	  (setf y (convert-ordonnee))
	  (if (and (not (= x 8)) (not (= y 8)))
	      (if (jouer-coup-sans-test tab joueur x y)
		(setf coupvalide T)
		(if affichage
		    (progn
		      (format t "Coup non valide~%")
		      (format t "Nouveau coup : ")))
	      )
	      (if affichage
		  (format t "Coup impossible car entrée incorrecte, veuillez entrer une lettre entre A et H et un chiffre entre 1 et 8 inclus~%Nouveau coup : "))))
	(if affichage
	    (format t "Le joueur ne peut pas jouer~%"))
	)))

(defun compter-pions (tab joueur)
  (let ((pions 0))
    (do ((i 0 (+ i 1)))((= i 8))
      (do ((j 0 (+ j 1)))((= j 8))
	(if (= (aref tab i j) joueur)
	    (setf pions (+ pions 1)))))
    pions))

(defun char-abscisse (x)
  (assert (and (< x 8) (>= x 0)))
  (if (= 0 x)
      (write-char #\A)
      (if (= 1 x)
	  (write-char #\B)
	  (if (= 2 x)
	      (write-char #\C)
	      (if (= 3 x)
		  (write-char #\D)
		  (if (= 4 x)
		      (write-char #\E)
		      (if (= 5 x)
			  (write-char #\F)
			  (if (= 6 x)
			      (write-char #\G)
			      (if (= 7 x)
				  (write-char #\H))))))))))


(defun char-ordonnee (y)
  (assert (and (< y 8) (>= y 0)))
  (if (= 0 y)
      (write-char #\1)
      (if (= 1 y)
	  (write-char #\2)
	  (if (= 2 y)
	      (write-char #\3)
	      (if (= 3 y)
		  (write-char #\4)
		  (if (= 4 y)
		      (write-char #\5)
		      (if (= 5 y)
			  (write-char #\6)
			  (if (= 6 y)
			      (write-char #\7)
			      (if (= 7 y)
				  (write-char #\8))))))))))

;Jouer coup de l'IA (si strat = 0 -> meilleurs coups de l'adv, si strat = 1 -> tous les coups de l'adv)
(defun jouer-coup-ia (tab joueur profondeur &key(ecrirecoup T) (affichage NIL) (strat 0))
  (let ((x 8)(y 8)(max 0)(valcoup 0))
    (if (not (ne-peut-pas-jouer tab joueur))
	(progn
	  (do ((i 0 (+ i 1)))((= i 8))
	    (do ((j 0 (+ j 1)))((= j 8))
	      (if (= (aref tab i j) 0)
		  (if (coup-valide tab joueur i j)
		      (progn
			(if (= 0 strat)
			    (setf valcoup (eval-coup-best-adv tab joueur profondeur i j))
			    (if (= 1 strat)
				(setf valcoup (eval-coup-all-adv tab joueur profondeur i j))))
		        
			(if (> valcoup max)
			    (progn
			      (setf max valcoup)
			      (setf x i)
			      (setf y j))))))))
	  (if ecrirecoup
	      (progn
		(char-abscisse x)
		(char-ordonnee y)))
	  (jouer-coup-sans-test tab joueur x y)
	  )
	(if affichage
	    (format t "L'ia ne peut pas jouer~%")))))


;Evalue un coup avec choix des meilleurs coups de l'adversaire
(defun eval-coup-best-adv (tab joueur profondeur i j)
  (let ((tab2 (clone tab))(nbrcoups 0)(sommecoups 0))
    (jouer-coup-sans-test tab2 joueur i j)
    (if (fin-partie tab2)
	(if (victoire tab2 joueur)
	    valeurvictoire
	    1)
	(if (or (= profondeur 0) (= profondeur -1)) 
	    (eval-tab tab2 joueur)
	    (progn
	      (jouer-coup-ia tab2 (adversaire joueur) (- profondeur 1) :ecrirecoup NIL)
	      (if (= 1 profondeur)
		  (eval-tab tab2 joueur)
		  (progn
		    (do ((x 0 (+ x 1)))((= x 8))
		      (do ((y 0 (+ y 1)))((= y 8))
			(if (coup-valide tab2 joueur x y)
			    (progn
			      (setf sommecoups (+ sommecoups (eval-coup-best-adv tab2 joueur (- profondeur 2) x y)))
			      (setf nbrcoups (+ nbrcoups 1))))))))
		  (if (= nbrcoups 0)
		      1
		      (/ sommecoups nbrcoups)))))))

;Evalue un coup avec tous les coups de l'adversaire
(defun eval-coup-all-adv (tab joueur profondeur i j)
  (let ((tab2 (clone tab))(tab3 (clone tab))(nbrcoups 0)(sommecoups 0))
    (jouer-coup-sans-test tab2 joueur i j)
    (if (fin-partie tab2)
	(if (victoire tab2 joueur)
	    valeurvictoire
	    1)
	(if (or (= profondeur 0) (= profondeur -1)) 
	    (eval-tab tab2 joueur)
	    (progn
	      (do ((l 0 (+ l 1)))((= l 8))
		(do ((h 0 (+ h 1)))((= h 8))
		  (setf tab3 (clone tab2))
		  (if (jouer-coup-sans-test tab3 (adversaire joueur) h l)
			(if (= 1 profondeur)
			    (progn
			      (setf sommecoups (+ sommecoups (eval-tab tab3 joueur)))
			      (setf nbrcoups (+ nbrcoups 1)))
			    (progn
			      (do ((x 0 (+ x 1)))((= x 8))
				(do ((y 0 (+ y 1)))((= y 8))
				  (if (coup-valide tab3 joueur x y)
				      (progn
					(setf sommecoups (+ sommecoups (eval-coup-best-adv tab3 joueur (- profondeur 2) x y)))
					(setf nbrcoups (+ nbrcoups 1)))))))))))
	      (if (= nbrcoups 0)
		  1
		  (/ sommecoups nbrcoups)))))))


(defun eval-tab (tab joueur)
  (let ((valtab (compter-pions tab joueur)))
    (if (= joueur (aref tab 0 0))
	(setf valtab (+ valtab 5))
	(if (= 0 (aref tab 0 0))
	    (setf valtab (+ valtab 2))))
    (if (= joueur (aref tab 0 7))
	(setf valtab (+ valtab 5))
	(if (= 0 (aref tab 0 7))
	    (setf valtab (+ valtab 2))))
    (if (= joueur (aref tab 7 0))
	(setf valtab (+ valtab 5))
	(if (= 0 (aref tab 7 0))
	    (setf valtab (+ valtab 2))))
    (if (= joueur (aref tab 7 7))
	(setf valtab (+ valtab 5))
	(if (= 0 (aref tab 7 7))
	    (setf valtab (+ valtab 2))))
    (do ((x 1 (+ x 1)))((= x 7))
      (if (= joueur (aref tab 0 x))
	(setf valtab (+ valtab 1)))
      (if (= joueur (aref tab 7 x))
	(setf valtab (+ valtab 1)))
      (if (= joueur (aref tab x 0))
	(setf valtab (+ valtab 1)))
      (if (= joueur (aref tab x 7))
	(setf valtab (+ valtab 1))))
    valtab))
    

(defun victoire (tab joueur)
  (if (> (compter-pions tab joueur) (compter-pions tab (adversaire joueur)))
      T
      NIL))

;impossible à utiliser : trop long à répondre
(defun eval-coup-max-victoires (tab joueur i j)
  (compter-victoires tab joueur i j))

(defun compter-victoires (tab joueur i j)
  (assert (coup-valide tab joueur i j))
  (let ((tab2 (clone tab)))
    (jouer-coup-sans-test tab2 joueur i j)
    (if (fin-partie tab2)
	(progn
	  (if (> (compter-pions tab2 joueur) (compter-pions tab2 (adversaire joueur)))
	      1
	      0))
	(compter-victoires-2 tab2 joueur))))

(defun compter-victoires-2 (tab joueur)
  (let ((tab2 (clone tab)) (somme 0))
    (do ((i 0 (+ i 1)))((= i 8))
      (do ((j 0 (+ j 1)))((= j 8))
	(if (coup-valide tab2 (adversaire joueur) i j)
	    (progn
	      (jouer-coup-sans-test tab2 (adversaire joueur) i j)
	      (if (fin-partie tab)
		    (if (> (compter-pions tab2 joueur) (compter-pions tab2 (adversaire joueur)))
			(setf somme (+ somme 1)))
		    (do ((x 0 (+ x 1)))((= x 8))
		      (do ((y 0 (+ y 1)))((= y 8))
			(if (coup-valide tab2 joueur x y)
			    (setf somme (+ somme (compter-victoires tab2 joueur x y)))))))
	      (setf tab2 (clone tab))))))
    somme))


(defun clone (tab)
  (let ((tab2 (make-array '(8 8))))
    (do ((x 0 (+ x 1)))((= x 8))
      (do ((y 0 (+ y 1)))((= y 8))
	(setf (aref tab2 x y) (aref tab x y))))
    tab2))

