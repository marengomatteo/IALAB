(defmodule DUMB_COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))


(defglobal ?*pesoRP* = 1)
(defglobal ?*pesoMP* = 1)
(defglobal ?*pesoMISSING* = 1)

(deftemplate combination-weight (slot step) (slot pos) (slot weight) (slot color))

(deftemplate combination (slot step) (multislot comb))

(deftemplate trovati-quattro (slot trovato))

(deftemplate peso (slot pos) (slot red) (slot blue) (slot yellow) (slot white) (slot black) (slot purple) (slot green) (slot orange))

(deffacts pesi
  (peso (pos 1) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 2) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 3) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 4) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
)

(deftemplate control
   (slot counter))

(defrule initialize-counter
   (declare (salience 100))
   (not (control))
   =>
   (assert (control (counter 0)))
)

(defrule init-computer
    (status (step 0) (mode computer))
    ?control <- (control (counter 0))
    =>
    ;; Inizializza la lista dei colori disponibili
    (bind ?colors (create$ red blue green yellow orange white black purple))
    ;; Inizializza la lista del nuovo tentativo
    (bind ?new-guess (create$)) 
    ;; Genera il tentativo automaticamente
    (loop-for-count (?i 4)
        (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
        (bind ?colors (delete-member$ ?colors ?random-color)) 
        ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
        (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
    )
    ;; Inserisce il nuovo tentativo
    (assert (guess (step 0) (g ?new-guess)))
    ;; stampa il tentativo      
    (printout t "Computer's guess at step " 0 ": " crlf) 
    (printout t (implode$ ?new-guess) crlf)
    (pop-focus)
)

(defrule init-computer2
  (status (step 1) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 1) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer3
  (status (step 2) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 2) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer4
  (status (step 3) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 3) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer5
  (status (step 4) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 4) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer6
  (status (step 5) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 5) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer7
  (status (step 6) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 6) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer8
  (status (step 7) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 7) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer9
  (status (step 8) (mode computer))
  ?control <- (control (counter 0))
  =>
  ;; Inizializza la lista dei colori disponibili
  (bind ?colors (create$ red blue green yellow orange white black purple))
  ;; Inizializza la lista del nuovo tentativo
  (bind ?new-guess (create$))
  ;; Genera il tentativo automaticamente
  (loop-for-count (?i 4)
    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
    (bind ?colors (delete-member$ ?colors ?random-color)) 
    ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
  )
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 8) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(deffunction find-max-weight (?pos ?colors)
   (bind ?max-weight -10000000)
   (bind ?max-color "none")

   (foreach ?color (create$ ?colors)
        (bind ?current-weight (fact-slot-value ?pos ?color))
        (if (> ?current-weight ?max-weight)
            then
            (bind ?max-weight ?current-weight)
            (bind ?max-color ?color)
        )
    )

    (bind ?new-colors (delete-member$ ?colors ?max-color)) 
    (return (create$ ?max-color ?max-weight $?new-colors))
)

(defrule trovato-quattro-mp-4 (declare (salience 85))   
    (status (step ?s) (mode computer))
    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
    (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
    (test (= ?prev-s (- ?s 1)))
    (test (= ?mp 4))
    =>
    ;; Inserisce il nuovo tentativo
    (assert (guess (step ?s) (g ?c4 ?c1 ?c2 ?c3)))
    ;; Inserisce il nuovo tentativo
    (printout t "Computer's guess at step " ?s ": " crlf)
    (printout t ?c4 " " ?c1 " " ?c2 " " ?c3 crlf)
    (pop-focus)
)

(defrule trovato-quattro-mp-3 (declare (salience 85))   
    (status (step ?s) (mode computer))
    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
    (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
    (test (= ?prev-s (- ?s 1)))
    (test (= (+ ?rp ?mp) 4))
    (test (= ?mp 3))
    =>

    ;; Inserisce il nuovo tentativo
    (assert (guess (step ?s) (g ?c1 ?c4 ?c2 ?c3)))
    ;; Inserisce il nuovo tentativo
    (printout t "Computer's guess at step " ?s ": " crlf)
    (printout t ?c1 " " ?c4 " " ?c2 " " ?c3 crlf)
    (pop-focus)
)

(defrule trovato-quattro-mp-2 (declare (salience 85))   
    (status (step ?s) (mode computer))
    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
    (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
    (test (= ?prev-s (- ?s 1)))
    (test (= (+ ?rp ?mp) 4))
    (test (= ?mp 2))
    =>
    ;; Inserisce il nuovo tentativo
    (assert (guess (step ?s) (g ?c1 ?c3 ?c2 ?c4)))
    ;; Inserisce il nuovo tentativo
    (printout t "Computer's guess at step " ?s ": " crlf)
    (printout t ?c1 " " ?c3 " " ?c2 " " ?c4 crlf)
    (pop-focus)
)

(defrule trovato-zero (declare (salience 85))   
    (status (step ?s) (mode computer))
    (guess (step ?prev-s) (g ?gc1 ?gc2 ?gc3 ?gc4))
    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
    (test (= ?prev-s (- ?s 1)))
    (test (= (+ ?rp ?mp) 0))
    ?pos1 <- (peso (pos 1))
    ?pos2 <- (peso (pos 2))
    ?pos3 <- (peso (pos 3))
    ?pos4 <- (peso (pos 4))
    =>

    (bind ?colors (create$ red blue green yellow orange white black purple))
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ (delete-member$ ?colors ?gc1) ?gc2) ?gc3) ?gc4))

    ;; Inizializza la lista del nuovo tentativo
    (bind ?new-guess (create$)) 
    ;; Genera il tentativo automaticamente
    (bind ?result1 (find-max-weight ?pos1 ?colors))
    (bind ?c1 (nth$ 1 ?result1))
    (bind ?w1 (nth$ 2 ?result1))
    (bind ?colors (create$ (subseq$ ?result1 3 (length$ ?result1))))
    (assert (combination-weight (step ?s) (pos 1) (weight ?w1) (color ?c1)))

    (bind ?result2 (find-max-weight ?pos2 ?colors))
    (bind ?c2 (nth$ 1 ?result2))
    (bind ?w2 (nth$ 2 ?result2))
    (bind ?colors (create$ (subseq$ ?result2 3 (length$ ?result2))))
    (assert (combination-weight (step ?s) (pos 2) (weight ?w2) (color ?c1)))
   
    (bind ?result3 (find-max-weight ?pos3 ?colors))
    (bind ?c3 (nth$ 1 ?result3))
    (bind ?w3 (nth$ 2 ?result3))
    (bind ?colors (create$ (subseq$ ?result3 3 (length$ ?result3))))
    (assert (combination-weight (step ?s) (pos 3) (weight ?w3) (color ?c3)))
   
    (bind ?result4 (find-max-weight ?pos4 ?colors))
    (bind ?c4 (nth$ 1 ?result4))
    (bind ?w4 (nth$ 2 ?result4))
    (assert (combination-weight (step ?s) (pos 4) (weight ?w4) (color ?c4)))

    ;; Inserisce il nuovo tentativo
    (assert (guess (step ?s) (g ?c1 ?c2 ?c3 ?c4)))
    ;; stampa il tentativo      
    (printout t "Computer's guess at step " ?s ": " crlf) 
    (printout t ?c1 " " ?c2 " " ?c3 " " ?c4 crlf)
    (pop-focus)
)


(defrule computer-player (declare (salience 70))
  (status (step ?s) (mode computer))
  (guess (step ?prev-s) (g $?k))
  (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
  (test (= ?prev-s (- ?s 1)))
  ?control <- (control (counter 0))
  ?pos1 <- (peso (pos 1))
  ?pos2 <- (peso (pos 2))
  ?pos3 <- (peso (pos 3))
  ?pos4 <- (peso (pos 4))
  =>

    (bind ?colors (create$ red blue green yellow orange white black purple))

    (bind ?result1 (find-max-weight ?pos1 ?colors))
    (bind ?c1 (nth$ 1 ?result1))
    (bind ?w1 (nth$ 2 ?result1))
    (assert (combination-weight (step ?s) (pos 1) (weight ?w1) (color ?c1)))

    (bind ?result2 (find-max-weight ?pos2 ?colors))
    (bind ?c2 (nth$ 1 ?result2))
    (bind ?w2 (nth$ 2 ?result2))
    (assert (combination-weight (step ?s) (pos 2) (weight ?w2) (color ?c1)))
   
    (bind ?result3 (find-max-weight ?pos3 ?colors))
    (bind ?c3 (nth$ 1 ?result3))
    (bind ?w3 (nth$ 2 ?result3))
    (assert (combination-weight (step ?s) (pos 3) (weight ?w3) (color ?c3)))
   
    (bind ?result4 (find-max-weight ?pos4 ?colors))
    (bind ?c4 (nth$ 1 ?result4))
    (bind ?w4 (nth$ 2 ?result4))
    (assert (combination-weight (step ?s) (pos 4) (weight ?w4) (color ?c4)))
   
    (assert (combination (step ?s) (comb ?c1 ?c2 ?c3 ?c4)))
  
)

;; ----------------------------
;    TUTTA C1 e C2
;; ----------------------------
(defrule test-combination-C1-c2 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?pos2 <- (peso (pos 2))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c2))
    (test (>= ?w1 ?w2))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c1) ?cc3) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos2 ?colors))
    (bind ?new-c2 (nth$ 1 ?result1))
    (bind ?new-w2 (nth$ 2 ?result1))

    (modify ?cw2 (color ?new-c2) (weight ?new-w2))
    (modify ?comb (comb ?cc1 ?new-c2 ?cc3 ?cc4))
)

(defrule test-combination-c1-C2 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?pos1 <- (peso (pos 1))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c2))
    (test (< ?w1 ?w2))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c2) ?cc3) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos1 ?colors))
    (bind ?new-c1 (nth$ 1 ?result1))
    (bind ?new-w1 (nth$ 2 ?result1))

    (modify ?cw1 (color ?new-c1) (weight ?new-w1))
    (modify ?comb (comb ?new-c1 ?cc2 ?cc3 ?cc4))
)

;; ----------------------------
;    TUTTA C1 e C3
;; ----------------------------
(defrule test-combination-C1-c3 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?pos3 <- (peso (pos 3))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c3))
    (test (>= ?w1 ?w3))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c1) ?cc2) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos3 ?colors))
    (bind ?new-c3 (nth$ 1 ?result1))
    (bind ?new-w3 (nth$ 2 ?result1))

    (modify ?cw3 (color ?new-c3) (weight ?new-w3))
    (modify ?comb (comb ?cc1 ?cc2 ?new-c3 ?cc4))
)

(defrule test-combination-c1-C3 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?pos1 <- (peso (pos 1))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c3))
    (test (< ?w1 ?w3))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c3) ?cc2) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos1 ?colors))
    (bind ?new-c1 (nth$ 1 ?result1))
    (bind ?new-w1 (nth$ 2 ?result1))

    (modify ?cw1 (color ?new-c1) (weight ?new-w1))
    (modify ?comb (comb ?new-c1 ?cc2 ?c3 ?cc4))
)

;; ----------------------------
;    TUTTA C1 e C4
;; ----------------------------
(defrule test-combination-C1-c4 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos4 <- (peso (pos 4))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c4))
    (test (>= ?w1 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c1) ?cc2) ?cc3))
    
    (bind ?result1 (find-max-weight ?pos4 ?colors))
    (bind ?new-c4 (nth$ 1 ?result1))
    (bind ?new-w4 (nth$ 2 ?result1))

    (modify ?cw4 (color ?new-c4) (weight ?new-w4))
    (modify ?comb (comb ?cc1 ?cc2 ?cc3 ?new-c4))
)

(defrule test-combination-c1-C4 (declare (salience 65))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos1 <- (peso (pos 1))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c1 ?c4))
    (test (< ?w1 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc3) ?cc2) ?c4))
    
    (bind ?result1 (find-max-weight ?pos1 ?colors))
    (bind ?new-c1 (nth$ 1 ?result1))
    (bind ?new-w1 (nth$ 2 ?result1))

    (modify ?cw1 (color ?new-c1) (weight ?new-w1))
    (modify ?comb (comb ?new-c1 ?cc2 ?cc3 ?c4))
)

;; ----------------------------
;    TUTTA C2 e C3
;; ----------------------------
(defrule test-combination-C2-c3 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?pos3 <- (peso (pos 3))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c2 ?c3))
    (test (>= ?w2 ?w3))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc1) ?c2) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos3 ?colors))
    (bind ?new-c3 (nth$ 1 ?result1))
    (bind ?new-w3 (nth$ 2 ?result1))

    (modify ?cw3 (color ?new-c3) (weight ?new-w3))
    (modify ?comb (comb ?cc1 ?c2 ?new-c3 ?cc4))
)

(defrule test-combination-c2-C3 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?pos2 <- (peso (pos 2))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c2 ?c3))
    (test (< ?w2 ?w3))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?c3) ?cc1) ?cc4))
    
    (bind ?result1 (find-max-weight ?pos2 ?colors))
    (bind ?new-c2 (nth$ 1 ?result1))
    (bind ?new-w2 (nth$ 2 ?result1))

    (modify ?cw2 (color ?new-c2) (weight ?new-w2))
    (modify ?comb (comb ?cc1 ?new-c2 ?c3 ?cc4))
)

;; ----------------------------
;    TUTTA C2 e C4
;; ----------------------------
(defrule test-combination-C2-c4 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos4 <- (peso (pos 4))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c2 ?c4))
    (test (>= ?w2 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc1) ?c2) ?cc3))
    
    (bind ?result1 (find-max-weight ?pos4 ?colors))
    (bind ?new-c4 (nth$ 1 ?result1))
    (bind ?new-w4 (nth$ 2 ?result1))

    (modify ?cw4 (color ?new-c4) (weight ?new-w4))
    (modify ?comb (comb ?cc1 ?cc2 ?cc3 ?new-c4))
)

(defrule test-combination-c2-C4 (declare (salience 65))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos2 <- (peso (pos 2))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c2 ?c4))
    (test (< ?w2 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc1) ?cc3) ?c4))
    
    (bind ?result1 (find-max-weight ?pos2 ?colors))
    (bind ?new-c2 (nth$ 1 ?result1))
    (bind ?new-w2 (nth$ 2 ?result1))

    (modify ?cw2 (color ?new-c2) (weight ?new-w2))
    (modify ?comb (comb ?cc1 ?new-c2 ?cc3 ?c4))
)

;; ----------------------------
;    TUTTA C3 e C4
;; ----------------------------

(defrule test-combination-C3-c4 (declare (salience 65))
    (status (step ?s) (mode computer))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos4 <- (peso (pos 4))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c3 ?c4))
    (test (>= ?w3 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc1) ?cc2) ?c3))
    
    (bind ?result1 (find-max-weight ?pos4 ?colors))
    (bind ?new-c4 (nth$ 1 ?result1))
    (bind ?new-w4 (nth$ 2 ?result1))

    (modify ?cw4 (color ?new-c4) (weight ?new-w4))
    (modify ?comb (comb ?cc1 ?cc2 ?cc3 ?new-c4))
)

(defrule test-combination-c3-C4 (declare (salience 65))
 (status (step ?s) (mode computer))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos3 <- (peso (pos 3))
    ?comb <- (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    (test (eq ?c3 ?c4))
    (test (< ?w3 ?w4))
    => 
    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ ?colors ?cc2) ?cc1) ?c4))
    
    (bind ?result1 (find-max-weight ?pos3 ?colors))
    (bind ?new-c3 (nth$ 1 ?result1))
    (bind ?new-w3 (nth$ 2 ?result1))

    (modify ?cw3 (color ?new-c3) (weight ?new-w3))
    (modify ?comb (comb ?cc1 ?cc2 ?new-c3 ?c4))
)


(defrule assert-guess (declare (salience 60))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (step ?s) (pos 1) (color ?c1))
    ?cw2 <- (combination-weight (step ?s) (pos 2) (color ?c2))
    ?cw3 <- (combination-weight (step ?s) (pos 3) (color ?c3))
    ?cw4 <- (combination-weight (step ?s) (pos 4) (color ?c4))
    (not (guess (g ?c1 ?c2 ?c3 ?c4)))
    =>

    (retract ?cw1)
    (retract ?cw2)
    (retract ?cw3)
    (retract ?cw4)
    (assert (guess (step ?s) (g ?c1 ?c2 ?c3 ?c4)))
    ;; Inserisce il nuovo tentativo
    (printout t "Computer's guess at step " ?s ": " crlf)
    (printout t ?c1 " " ?c2 " " ?c3 " " ?c4 crlf)
    (pop-focus)
)

(defrule assert-new-guess (declare (salience 60))
    (status (step ?s) (mode computer))
    ?cw1 <- (combination-weight (pos 1) (step ?s) (color ?c1) (weight ?w1))
    ?cw2 <- (combination-weight (pos 2) (step ?s) (color ?c2) (weight ?w2))
    ?cw3 <- (combination-weight (pos 3) (step ?s) (color ?c3) (weight ?w3))
    ?cw4 <- (combination-weight (pos 4) (step ?s) (color ?c4) (weight ?w4))
    ?pos1 <- (peso (pos 1))
    ?pos2 <- (peso (pos 2))
    ?pos3 <- (peso (pos 3))
    ?pos4 <- (peso (pos 4))
    (combination (step ?s) (comb ?cc1 ?cc2 ?cc3 ?cc4))
    =>

    (bind ?colors red blue yellow white black purple green orange)
    (bind ?colors (delete-member$ (delete-member$ (delete-member$ (delete-member$ ?colors ?c4) ?c3) ?c2) ?c1))

    (bind ?min-weight (min ?w1 ?w2 ?w3 ?w4))

    ; mi trovo il combination-weight che ha min-weight
    (if (= ?min-weight ?w1) then (bind ?min-cw ?cw1))
    (if (= ?min-weight ?w2) then (bind ?min-cw ?cw2))
    (if (= ?min-weight ?w3) then (bind ?min-cw ?cw3))
    (if (= ?min-weight ?w4) then (bind ?min-cw ?cw4))

    ; prendo min-pos
    (bind ?min-pos (fact-slot-value ?min-cw pos))

    ; mi trovo il peso con pos corretta 
    (if (= ?min-pos 1) then (bind ?pos ?pos1))
    (if (= ?min-pos 2) then (bind ?pos ?pos2))
    (if (= ?min-pos 3) then (bind ?pos ?pos3))
    (if (= ?min-pos 4) then (bind ?pos ?pos4))


    (bind ?unique-guess-found FALSE)
    (while (not ?unique-guess-found)
        (bind ?result (find-max-weight ?pos ?colors))
        (bind ?new-c (nth$ 1 ?result))
        (bind ?new-w (nth$ 2 ?result))

        ; mi genero la nuova guess
        (if (= ?min-pos 1) then (bind ?new-guess (create$ ?new-c ?c2 ?c3 ?c4)))
        (if (= ?min-pos 2) then (bind ?new-guess (create$ ?c1 ?new-c ?c3 ?c4)))
        (if (= ?min-pos 3) then (bind ?new-guess (create$ ?c1 ?c2 ?new-c ?c4)))
        (if (= ?min-pos 4) then (bind ?new-guess (create$ ?c1 ?c2 ?c3 ?new-c)))

        ; Controllo se esiste già un fatto guess con questa combinazione
       (if (not (any-factp ((?g guess)) (eq ?g:g ?new-guess)))
            then 
            (bind ?unique-guess-found TRUE)
            else
            ; Se la combinazione esiste già, controlliamo la lunghezza di ?colors
            (if (> (length$ ?colors) 1)
                then
                (bind ?colors (delete-member$ ?colors ?new-c))
                else 
                
                (bind ?new-guess (create$)) 
                (bind ?colors red blue yellow white black purple green orange)
                (loop-for-count (?i 4)
                    (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
                    (bind ?colors (delete-member$ ?colors ?random-color)) 
                    (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
                )
                (bind ?unique-guess-found TRUE)
            )
        )
    )
    
    (retract ?cw1)
    (retract ?cw2)
    (retract ?cw3)
    (retract ?cw4)
    (assert (guess (step ?s) (g ?new-guess)))
    ;; Inserisce il nuovo tentativo
    (printout t "Computer's guess at step " ?s ": " crlf)
    (printout t (implode$ ?new-guess) crlf)
    
    (pop-focus)
)


(defrule update-weights-rp-pos1-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
    (not (updated-weight red 1 right-placed ?prev-s)) 
   =>
        (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))

        
        (assert (updated-weight red 1 right-placed ?prev-s))
)

(defrule update-weights-rp-pos1-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
    (not (updated-weight blue 1 right-placed ?prev-s))  

   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
    (bind ?current-value1 (fact-slot-value ?pos blue))
    (bind ?new-value1 (+ ?current-value1 ?weight-increment))
    (modify ?pos (blue ?new-value1))
    (assert (updated-weight blue 1 right-placed ?prev-s))
)

(defrule update-weights-rp-pos1-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   (not (updated-weight green 1 right-placed ?prev-s))  
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
    (bind ?current-value1 (fact-slot-value ?pos green))
    (bind ?new-value1 (+ ?current-value1 ?weight-increment))
    (modify ?pos (green ?new-value1))
    (assert (updated-weight green 1 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos1-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
    (not (updated-weight yellow 1 right-placed ?prev-s))  

   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
      (assert (updated-weight yellow 1 right-placed ?prev-s))

)

(defrule update-weights-rp-pos1-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
    (not (updated-weight black 1 right-placed ?prev-s))  

   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    (assert (updated-weight black 1 right-placed ?prev-s))

 
)

(defrule update-weights-rp-pos1-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
    (not (updated-weight white 1 right-placed ?prev-s))  
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    (assert (updated-weight white 1 right-placed ?prev-s))

  
)

(defrule update-weights-rp-pos1-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   (not (updated-weight purple 1 right-placed ?prev-s))  
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    (assert (updated-weight purple 1 right-placed ?prev-s))

)

(defrule update-weights-rp-pos1-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
      (not (updated-weight orange 1 right-placed ?prev-s))  

   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    (assert (updated-weight orange 1 right-placed ?prev-s))


)

(defrule update-weights-rp-pos2-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
    (not (updated-weight red 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    
    (assert (updated-weight red  2 right-placed ?prev-s))
  
)

(defrule update-weights-rp-pos2-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight blue 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    (assert (updated-weight blue  2 right-placed ?prev-s))
   
)

(defrule update-weights-rp-pos2-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight green 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
  
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    (assert (updated-weight green  2 right-placed ?prev-s))
   
)

(defrule update-weights-rp-pos2-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight yellow 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   

        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    (assert (updated-weight yellow  2 right-placed ?prev-s))
  
)

(defrule update-weights-rp-pos2-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight black 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    (assert (updated-weight black  2 right-placed ?prev-s))
  
)

(defrule update-weights-rp-pos2-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight white 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    (assert (updated-weight white  2 right-placed ?prev-s))

)

(defrule update-weights-rp-pos2-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight purple 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    (assert (updated-weight purple  2 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos2-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
      (not (updated-weight orange 2 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
 
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    (assert (updated-weight orange  2 right-placed ?prev-s))
  
)

(defrule update-weights-rp-pos3-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
      (not (updated-weight red 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    

        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    (assert (updated-weight red  3 right-placed ?prev-s))

)

(defrule update-weights-rp-pos3-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight blue 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    (assert (updated-weight blue 3 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos3-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3)) 
   (not (updated-weight green 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
        
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    
    (assert (updated-weight green  3 right-placed ?prev-s))
)

(defrule update-weights-rp-pos3-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight yellow 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
 
    (bind ?current-value1 (fact-slot-value ?pos yellow))
    (bind ?new-value1 (+ ?current-value1 ?weight-increment))
    (modify ?pos (yellow ?new-value1))
    (assert (updated-weight yellow  3 right-placed ?prev-s))

)

(defrule update-weights-rp-pos3-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight black 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    
    (assert (updated-weight black  3 right-placed ?prev-s))
)

(defrule update-weights-rp-pos3-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight white 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    (assert (updated-weight white  3 right-placed ?prev-s))
   
)

(defrule update-weights-rp-pos3-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight purple 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    (assert (updated-weight purple  3 right-placed ?prev-s))
  
)

(defrule update-weights-rp-pos3-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
    (not (updated-weight orange 3 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    (assert (updated-weight orange  3 right-placed ?prev-s))
    
)


(defrule update-weights-rp-pos4-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
    (not (updated-weight red 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    (assert (updated-weight red  4 right-placed ?prev-s))
)

(defrule update-weights-rp-pos4-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
    (not (updated-weight blue 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
   
    (assert (updated-weight blue  4 right-placed ?prev-s))
)

(defrule update-weights-rp-pos4-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
     (not (updated-weight green 4 right-placed ?prev-s)) 

   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
         (assert (updated-weight green 4 right-placed ?prev-s))
      
     
)

(defrule update-weights-rp-pos4-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
       (not (updated-weight yellow 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    (assert (updated-weight yellow  4 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos4-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
       (not (updated-weight black 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    (assert (updated-weight black  4 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos4-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
       (not (updated-weight white 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
     
    (assert (updated-weight white  4 right-placed ?prev-s))
)

(defrule update-weights-rp-pos4-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
       (not (updated-weight purple 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    (assert (updated-weight purple  4 right-placed ?prev-s))
    
)

(defrule update-weights-rp-pos4-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
       (not (updated-weight orange 4 right-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    (assert (updated-weight orange  4 right-placed ?prev-s))
     
)

(defrule update-weights-mp1-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight red 1 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

   
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
    
    (assert (updated-weight red  1 miss-placed ?prev-s))
    
)

(defrule update-weights-mp1-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight blue 1 miss-placed ?prev-s)) 
   =>

    (bind ?weight-increment (* ?*pesoMP* ?mp))
    


        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))
    (assert (updated-weight blue 1 miss-placed ?prev-s))


)

(defrule update-weights-mp1-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight green 1 miss-placed ?prev-s)) 
    
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

   
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
    (assert (updated-weight green  1 miss-placed ?prev-s))
  
)

(defrule update-weights-mp1-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight yellow 1 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
   
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
        (assert (updated-weight yellow  1 miss-placed ?prev-s))
  
)

(defrule update-weights-mp1-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight black 1 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
   
        (assert (updated-weight black  1 miss-placed ?prev-s))
)

(defrule update-weights-mp1-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight white 1 miss-placed ?prev-s)) 
     
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
      (assert (updated-weight white  1 miss-placed ?prev-s))

)

(defrule update-weights-mp1-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight purple 1 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
   
         (assert (updated-weight purple  1 miss-placed ?prev-s))
)

(defrule update-weights-mp1-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight orange 1 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
    (assert (updated-weight orange 1 miss-placed ?prev-s))
   
)

(defrule update-weights-mp2-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight red 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
      (assert (updated-weight red 2 miss-placed ?prev-s))
)

(defrule update-weights-mp2-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight blue 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))
      (assert (updated-weight blue 2 miss-placed ?prev-s))
      
)

(defrule update-weights-mp2-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight green 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

   
        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
      (assert (updated-weight green 2 miss-placed ?prev-s))
       
)

(defrule update-weights-mp2-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight yellow 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
      (assert (updated-weight yellow 2 miss-placed ?prev-s))
    
)

(defrule update-weights-mp2-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight black 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
     
      (assert (updated-weight black 2 miss-placed ?prev-s))
)

(defrule update-weights-mp2-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight white 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
      (assert (updated-weight white 2 miss-placed ?prev-s))
      
)

(defrule update-weights-mp2-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight purple 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
      (assert (updated-weight purple 2 miss-placed ?prev-s))
      
)

(defrule update-weights-mp2-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight orange 2 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
   
        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
     
      (assert (updated-weight orange 2 miss-placed ?prev-s))
)

(defrule update-weights-mp3-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight red 3 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
 
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos4 (red ?new-value4))
      (assert (updated-weight red 3 miss-placed ?prev-s))

)

(defrule update-weights-mp3-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight blue 3 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

    
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos4 (blue ?new-value4))
  
      (assert (updated-weight blue 3 miss-placed ?prev-s))
)

(defrule update-weights-mp3-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight green 3 miss-placed ?prev-s)) 
   
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos4 (green ?new-value4))
      (assert (updated-weight green 3 miss-placed ?prev-s))
   
)

(defrule update-weights-mp3-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight yellow 3 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos4 (yellow ?new-value4))
    (assert (updated-weight yellow 3 miss-placed ?prev-s))
)

(defrule update-weights-mp3-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight black 3 miss-placed ?prev-s)) 
   
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos4 (black ?new-value4))
      (assert (updated-weight black 3 miss-placed ?prev-s))
  
)

(defrule update-weights-mp3-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight white 3 miss-placed ?prev-s)) 
   
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos4 (white ?new-value4))
  
      (assert (updated-weight white 3 miss-placed ?prev-s))
)

(defrule update-weights-mp3-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight purple 3 miss-placed ?prev-s)) 
   
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos4 (purple ?new-value4))
      (assert (updated-weight purple 3 miss-placed ?prev-s))
 
)

(defrule update-weights-mp3-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight orange 3 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos4 (orange ?new-value4))
      (assert (updated-weight orange 3 miss-placed ?prev-s))
   
)

(defrule update-weights-mp4-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight red 4 miss-placed ?prev-s)) 
   
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
      (assert (updated-weight red 4 miss-placed ?prev-s))
   
)

(defrule update-weights-mp4-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight blue 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
      (assert (updated-weight blue 4 miss-placed ?prev-s))
 
)

(defrule update-weights-mp4-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight green 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
        
      (assert (updated-weight green 4 miss-placed ?prev-s))

)

(defrule update-weights-mp4-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight yellow 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
 
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
      (assert (updated-weight yellow 4 miss-placed ?prev-s))

)

(defrule update-weights-mp4-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight black 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
      (assert (updated-weight black 4 miss-placed ?prev-s))
   
)

(defrule update-weights-mp4-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight white 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
      (assert (updated-weight white 4 miss-placed ?prev-s))

)

(defrule update-weights-mp4-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight purple 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))
    
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
      (assert (updated-weight purple 4 miss-placed ?prev-s))

)

(defrule update-weights-mp4-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight orange 4 miss-placed ?prev-s)) 
   =>
    (bind ?weight-increment (* ?*pesoMP* ?mp))

        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
      (assert (updated-weight orange 4 miss-placed ?prev-s))

)

(defrule update-weights-missing-red (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight red 1 missing ?prev-s)) 
 
     
   =>
    (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))
    
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
        (assert (updated-weight red 1 missing ?prev-s))
)

(defrule update-weights-missing-blue (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
     (not (updated-weight blue 1 missing ?prev-s)) 
     
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))
   
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))

    (assert (updated-weight blue 1 missing ?prev-s))

)  

(defrule update-weights-missing-green (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight green 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))


        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
      (assert (updated-weight green 1 missing ?prev-s))
)

(defrule update-weights-missing-yellow (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight yellow 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
      (assert (updated-weight yellow 1 missing ?prev-s))
    )


(defrule update-weights-missing-black (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight black 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))

    
        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
      (assert (updated-weight black 1 missing ?prev-s))
    )

(defrule update-weights-missing-white (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight white 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))
        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
      (assert (updated-weight white 1 missing ?prev-s))
    )

(defrule update-weights-missing-purple (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight purple 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))
    
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
        
      (assert (updated-weight purple 1 missing ?prev-s))
    )
(defrule update-weights-missing-orange (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
    (not (updated-weight orange 1 missing ?prev-s)) 
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
    (bind ?weight-increment (* ?*pesoMISSING* ?missing))

        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (- ?current-value1 ?weight-increment))
        (bind ?new-value2 (- ?current-value2 ?weight-increment))
        (bind ?new-value3 (- ?current-value3 ?weight-increment))
        (bind ?new-value4 (- ?current-value4 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
      (assert (updated-weight orange 1 missing ?prev-s))
    )

(defrule update-weights-missing-red-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
     (not (updated-weight red 1 missing-none ?prev-s)) 

   (test (= (+ ?rp ?mp) 0))

   =>

            (modify ?pos1 (red -99))
            (modify ?pos2 (red -99))
            (modify ?pos3 (red -99))
            (modify ?pos4 (red -99))
      (assert (updated-weight red 1 missing-none ?prev-s))
         
)

(defrule update-weights-missing-blue-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight blue 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (blue -99))
            (modify ?pos2 (blue -99))
            (modify ?pos3 (blue -99))
            (modify ?pos4 (blue -99))
             
    (assert (updated-weight blue 1 missing-none ?prev-s))
)

(defrule update-weights-missing-green-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight green 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>
  
            (modify ?pos1 (green -99))
            (modify ?pos2 (green -99))
            (modify ?pos3 (green -99))
            (modify ?pos4 (green -99))
    (assert (updated-weight green 1 missing-none ?prev-s))
            
)
(defrule update-weights-missing-yellow-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight yellow 1 missing-none ?prev-s)) 
    
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (yellow -99))
            (modify ?pos2 (yellow -99))
            (modify ?pos3 (yellow -99))
            (modify ?pos4 (yellow -99))
            
    (assert (updated-weight yellow 1 missing-none ?prev-s))

)
(defrule update-weights-missing-black-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight black 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (black -99))
            (modify ?pos2 (black -99))
            (modify ?pos3 (black -99))
            (modify ?pos4 (black -99))
     (assert (updated-weight black 1 missing-none ?prev-s))
            
)
(defrule update-weights-missing-white-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight white 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>

            (modify ?pos1 (white -99))
            (modify ?pos2 (white -99))
            (modify ?pos3 (white -99))
            (modify ?pos4 (white -99))
     (assert (updated-weight white 1 missing-none ?prev-s))
            
)

(defrule update-weights-missing-purple-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight purple 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (purple -99))
            (modify ?pos2 (purple -99))
            (modify ?pos3 (purple -99))
            (modify ?pos4 (purple -99))
            
     (assert (updated-weight purple 1 missing-none ?prev-s))
)
(defrule update-weights-missing-orange-none (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight orange 1 missing-none ?prev-s)) 
   (test (= (+ ?rp ?mp) 0))
   =>
        
            (modify ?pos1 (orange -99))
            (modify ?pos2 (orange -99))
            (modify ?pos3 (orange -99))
            (modify ?pos4 (orange -99))
            
     (assert (updated-weight orange 1 missing-none ?prev-s))
)
   

(defrule update-weights-missing-red-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
    (not (updated-weight red 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
     (assert (updated-weight red 1 missing-all ?prev-s))
        
)
(defrule update-weights-missing-blue-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight blue 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))
        
     (assert (updated-weight blue 1 missing-all ?prev-s))
)
(defrule update-weights-missing-green-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight green 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
        
     (assert (updated-weight green 1 missing-all ?prev-s))
)

(defrule update-weights-missing-yellow-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight yellow 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
     (assert (updated-weight yellow 1 missing-all ?prev-s))
        
)
(defrule update-weights-missing-black-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight black 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
     (assert (updated-weight black 1 missing-all ?prev-s))
        
)
(defrule update-weights-missing-white-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight white 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
     (assert (updated-weight white 1 missing-all ?prev-s))
)
(defrule update-weights-missing-purple-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight purple 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
     (assert (updated-weight purple 1 missing-all ?prev-s))
)



(defrule update-weights-missing-orange-all (declare (salience 90))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (not (updated-weight orange 1 missing-all ?prev-s)) 
   (test (= (+ ?rp ?mp) 4))
   =>

        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (+ ?current-value1 99))
        (bind ?new-value2 (+ ?current-value2 99))
        (bind ?new-value3 (+ ?current-value3 99))
        (bind ?new-value4 (+ ?current-value4 99))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
     (assert (updated-weight orange 1 missing-all ?prev-s))
)

