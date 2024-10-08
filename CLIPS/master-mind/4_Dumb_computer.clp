
(defmodule DUMB_COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate combination (slot step) (multislot colors))

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
  (assert (guess (step 0) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
   (modify ?control (counter 9))
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

(defrule computer-player
  (status (step ?s) (mode computer))
  (guess (step ?prev-s) (g $?k))
  (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
  (test (= ?prev-s (- ?s 1)))
  ?control <- (control (counter 0))
  ?pos1 <- (peso (pos 1) (red ?pred) (blue ?pblue) (yellow ?pyellow) (white ?pwhite) (black ?pblack) (purple ?ppurple) (green ?pgreen) (orange ?porange))
  ?pos2 <- (peso (pos 2) (red ?pred2) (blue ?pblue2) (yellow ?pyellow2) (white ?pwhite2) (black ?pblack2) (purple ?ppurple2) (green ?pgreen2) (orange ?porange2))
  ?pos3 <- (peso (pos 3) (red ?pred3) (blue ?pblue3) (yellow ?pyellow3) (white ?pwhite3) (black ?pblack3) (purple ?ppurple3) (green ?pgreen3) (orange ?porange3))
  ?pos4 <- (peso (pos 4) (red ?pred4) (blue ?pblue4) (yellow ?pyellow4) (white ?pwhite4) (black ?pblack4) (purple ?ppurple4) (green ?pgreen4) (orange ?porange4))
  =>
  ;; Trova il colore con il peso massimo per la posizione 1
  (bind ?max-color1 red)
  (bind ?max-weight1 ?pred)
  (if (> ?pblue ?max-weight1) then (bind ?max-color1 blue) (bind ?max-weight1 ?pblue))
  (if (> ?pgreen ?max-weight1) then (bind ?max-color1 green) (bind ?max-weight1 ?pgreen))
  (if (> ?pyellow ?max-weight1) then (bind ?max-color1 yellow) (bind ?max-weight1 ?pyellow))
  (if (> ?porange ?max-weight1) then (bind ?max-color1 orange) (bind ?max-weight1 ?porange))
  (if (> ?pwhite ?max-weight1) then (bind ?max-color1 white) (bind ?max-weight1 ?pwhite))
  (if (> ?pblack ?max-weight1) then (bind ?max-color1 black) (bind ?max-weight1 ?pblack))
  (if (> ?ppurple ?max-weight1) then (bind ?max-color1 purple) (bind ?max-weight1 ?ppurple))
  
  ;; Trova il colore con il peso massimo per la posizione 2
  (if (not (eq ?max-color1 red)) 
  then 
        (bind ?max-color2 red) 
        (bind ?max-weight2 ?pred2) 
  else  (bind ?max-color2 blue) 
        (bind ?max-weight2 ?pblue2))
  (if (and (not (eq ?max-color1 blue)) (> ?pblue2 ?max-weight2)) then (bind ?max-color2 blue) (bind ?max-weight2 ?pblue2))
  (if (and (not (eq ?max-color1 green)) (> ?pgreen2 ?max-weight2)) then (bind ?max-color2 green) (bind ?max-weight2 ?pgreen2))
  (if (and (not (eq ?max-color1 yellow)) (> ?pyellow2 ?max-weight2)) then (bind ?max-color2 yellow) (bind ?max-weight2 ?pyellow2))
  (if (and (not (eq ?max-color1 orange)) (> ?porange2 ?max-weight2)) then (bind ?max-color2 orange) (bind ?max-weight2 ?porange2))
  (if (and (not (eq ?max-color1 white)) (> ?pwhite2 ?max-weight2)) then (bind ?max-color2 white) (bind ?max-weight2 ?pwhite2))
  (if (and (not (eq ?max-color1 black)) (> ?pblack2 ?max-weight2)) then (bind ?max-color2 black) (bind ?max-weight2 ?pblack2))
  (if (and (not (eq ?max-color1 purple)) (> ?ppurple2 ?max-weight2)) then (bind ?max-color2 purple) (bind ?max-weight2 ?ppurple2))
  
  ;; Trova il colore con il peso massimo per la posizione 3
  (if (and (not (eq ?max-color1 red)) (not (eq ?max-color2 red))) 
  then 
        (bind ?max-color3 red) 
        (bind ?max-weight3 ?pred3) 
        
  else (if (and (not (eq ?max-color1 blue)) (not (eq ?max-color2 blue))) 
        then 
        (bind ?max-color3 blue) 
        (bind ?max-weight3 ?pblue3)
        else (bind ?max-color3 green) (bind ?max-weight3 ?pgreen3)))
  (if (and (not (eq ?max-color1 blue)) (not (eq ?max-color2 blue)) (> ?pblue3 ?max-weight3)) then (bind ?max-color3 blue) (bind ?max-weight3 ?pblue3))
  (if (and (not (eq ?max-color1 green)) (not (eq ?max-color2 green)) (> ?pgreen3 ?max-weight3)) then (bind ?max-color3 green) (bind ?max-weight3 ?pgreen3))
  (if (and (not (eq ?max-color1 yellow)) (not (eq ?max-color2 yellow)) (> ?pyellow3 ?max-weight3)) then (bind ?max-color3 yellow) (bind ?max-weight3 ?pyellow3))
  (if (and (not (eq ?max-color1 orange)) (not (eq ?max-color2 oramge)) (> ?porange3 ?max-weight3)) then (bind ?max-color3 orange) (bind ?max-weight3 ?porange3))
  (if (and (not (eq ?max-color1 white)) (not (eq ?max-color2 white)) (> ?pwhite3 ?max-weight3)) then (bind ?max-color3 white) (bind ?max-weight3 ?pwhite3))
  (if (and (not (eq ?max-color1 black)) (not (eq ?max-color2 black)) (> ?pblack3 ?max-weight3)) then (bind ?max-color3 black) (bind ?max-weight3 ?pblack3))
  (if (and (not (eq ?max-color1 purple)) (not (eq ?max-color2 purple)) (> ?ppurple3 ?max-weight3)) then (bind ?max-color3 purple) (bind ?max-weight3 ?ppurple3))
  
  ;; Trova il colore con il peso massimo per la posizione 4
  (if (and (not (eq ?max-color1 red)) (not (eq ?max-color2 red)) (not (eq ?max-color3 red))) 
  then (bind ?max-color4 red) (bind ?max-weight4 ?pred4)
  else (if (and (not (eq ?max-color1 blue)) (not (eq ?max-color2 blue)) (not (eq ?max-color3 blue))) 
        then (bind ?max-color4 blue) (bind ?max-weight4 ?pblue4)
        else (if (and (not (eq ?max-color1 green)) (not (eq ?max-color2 green)) (not (eq ?max-color3 green)))
            then (bind ?max-color4 green) (bind ?max-weight4 ?pgreen4)
            else (bind ?max-color4 yellow) (bind ?max-weight4 ?pyellow4))))

  (if (and (not (eq ?max-color1 blue)) (not (eq ?max-color2 blue)) (not (eq ?max-color3 blue)) (> ?pblue4 ?max-weight4)) then (bind ?max-color4 blue) (bind ?max-weight4 ?pblue4))
  (if (and (not (eq ?max-color1 green)) (not (eq ?max-color2 green)) (not (eq ?max-color3 green)) (> ?pgreen4 ?max-weight4)) then (bind ?max-color4 green) (bind ?max-weight4 ?pgreen4))
  (if (and (not (eq ?max-color1 yellow)) (not (eq ?max-color2 yellow)) (not (eq ?max-color3 yellow)) (> ?pyellow4 ?max-weight4)) then (bind ?max-color4 yellow) (bind ?max-weight4 ?pyellow4))
  (if (and (not (eq ?max-color1 orange)) (not (eq ?max-color2 orange)) (not (eq ?max-color3 orange)) (> ?porange4 ?max-weight4)) then (bind ?max-color4 orange) (bind ?max-weight4 ?porange4))
  (if (and (not (eq ?max-color1 white)) (not (eq ?max-color2 white)) (not (eq ?max-color3 white)) (> ?pwhite4 ?max-weight4)) then (bind ?max-color4 white) (bind ?max-weight4 ?pwhite4))
  (if (and (not (eq ?max-color1 black)) (not (eq ?max-color2 black)) (not (eq ?max-color3 black)) (> ?pblack4 ?max-weight4)) then (bind ?max-color4 black) (bind ?max-weight4 ?pblack4))
  (if (and (not (eq ?max-color1 purple)) (not (eq ?max-color2 purple)) (not (eq ?max-color3 purple)) (> ?ppurple4 ?max-weight4)) then (bind ?max-color4 purple) (bind ?max-weight4 ?ppurple4))


  ;; Inserisce il nuovo tentativo
  (assert (guess (step ?s) (g ?max-color1 ?max-color2 ?max-color3 ?max-color4)))

  (modify ?control (counter 9))
  
  ;; Stampa il tentativo
  (printout t "Computer's guess at step " ?s ": " crlf)
  (printout t ?max-color1 " " ?max-color2 " " ?max-color3 " " ?max-color4 crlf)
  (pop-focus)
)


(defrule update-weights-rp-pos1
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   ?control <- (control (counter 4))
   =>
    (bind ?weight-increment (* 3 ?rp))
    (printout t "Regola 1 attivata con counter = 4" crlf)

    (if (eq ?c1 red) then
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    )
    (if (eq ?c1 blue) then
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    )
    (if (eq ?c1 green) then
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    )
    (if (eq ?c1 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    )
    (if (eq ?c1 black) then
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    )
    (if (eq ?c1 white) then
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    )
    (if (eq ?c1 purple) then
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    )
    (if (eq ?c1 orange) then
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    )
    
    (modify ?control (counter 3))
    (printout t "Regola eseguita, counter impostato a 3" crlf)
)

(defrule update-weights-rp-pos2
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   ?control <- (control (counter 3))
   =>
    (bind ?weight-increment (* 3 ?rp))
    (printout t "Regola 2 attivata con counter = 3" crlf)

    (if (eq ?c2 red) then
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    )
    (if (eq ?c2 blue) then
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    )
    (if (eq ?c2 green) then
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    )
    (if (eq ?c2 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    )
    (if (eq ?c2 black) then
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    )
    (if (eq ?c2 white) then
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    )
    (if (eq ?c2 purple) then
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    )
    (if (eq ?c2 orange) then
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    )
    
    (modify ?control (counter 2))
    (printout t "Regola eseguita, counter impostato a 2" crlf)
)

(defrule update-weights-rp-pos3
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   ?control <- (control (counter 2))
   =>
    (bind ?weight-increment (* 3 ?rp))
    (printout t "Regola 3 attivata con counter = 2" crlf)

    (if (eq ?c3 red) then
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    )
    (if (eq ?c3 blue) then
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    )
    (if (eq ?c3 green) then
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    )
    (if (eq ?c3 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    )
    (if (eq ?c3 black) then
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    )
    (if (eq ?c3 white) then
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    )
    (if (eq ?c3 purple) then
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    )
    (if (eq ?c3 orange) then
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    )
    
    (modify ?control (counter 1))
    (printout t "Regola eseguita, counter impostato a 1" crlf)
)

(defrule update-weights-rp-pos4
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   ?control <- (control (counter 1))
   =>
    (bind ?weight-increment (* 3 ?rp))
    (printout t "Regola 4 attivata con counter = 1" crlf)

    (if (eq ?c4 red) then
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
    )
    (if (eq ?c4 blue) then
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    )
    (if (eq ?c4 green) then
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    )
    (if (eq ?c4 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    )
    (if (eq ?c4 black) then
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    )
    (if (eq ?c4 white) then
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
    )
    (if (eq ?c4 purple) then
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    )
    (if (eq ?c4 orange) then
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    )
    
    (modify ?control (counter 0))
    (printout t "Regola eseguita, counter impostato a 0" crlf)
)



(defrule update-weights-mp1
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   ?control <- (control (counter 5))
   =>
    (bind ?weight-increment (* 1 ?mp))
    (printout t "Regola mp attivata con counter = 5" crlf)

    (if (eq ?c1 red) then
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
    )
    (if (eq ?c1 blue) then
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))
    )
    (if (eq ?c1 green) then
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
    )
    (if (eq ?c1 yellow) then
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
    )
    (if (eq ?c1 black) then
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
    )
    (if (eq ?c1 white) then
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
    )
    (if (eq ?c1 purple) then
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
    )
    (if (eq ?c1 orange) then
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
    )
    
    (modify ?control (counter 4))
    (printout t "Regola eseguita mp counter impostato a 4" crlf)
)

(defrule update-weights-mp2
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   ?control <- (control (counter 6))
   =>
    (bind ?weight-increment (* 1 ?mp))
    (printout t "Regola mp attivata con counter = 6" crlf)

    (if (eq ?c2 red) then
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos3 (red ?new-value3))
        (modify ?pos4 (red ?new-value4))
    )
    (if (eq ?c2 blue) then
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos3 (blue ?new-value3))
        (modify ?pos4 (blue ?new-value4))
    )
    (if (eq ?c2 green) then
        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos3 (green ?new-value3))
        (modify ?pos4 (green ?new-value4))
    )
    (if (eq ?c2 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos3 (yellow ?new-value3))
        (modify ?pos4 (yellow ?new-value4))
    )
    (if (eq ?c2 black) then
        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos3 (black ?new-value3))
        (modify ?pos4 (black ?new-value4))
    )
    (if (eq ?c2 white) then
        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos3 (white ?new-value3))
        (modify ?pos4 (white ?new-value4))
    )
    (if (eq ?c2 purple) then
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos3 (purple ?new-value3))
        (modify ?pos4 (purple ?new-value4))
    )
    (if (eq ?c2 orange) then
        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos3 (orange ?new-value3))
        (modify ?pos4 (orange ?new-value4))
    )

    
    (modify ?control (counter 5))
    (printout t "Regola eseguita mp counter impostato a 5" crlf)
)

(defrule update-weights-mp3
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   ?control <- (control (counter 7))
   =>
    (bind ?weight-increment (* 1 ?mp))
    (printout t "Regola mp attivata con counter = 7" crlf)

    (if (eq ?c3 red) then
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value4 (fact-slot-value ?pos4 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos4 (red ?new-value4))
    )
    (if (eq ?c3 blue) then
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value4 (fact-slot-value ?pos4 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos4 (blue ?new-value4))
    )
    (if (eq ?c3 green) then
        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value4 (fact-slot-value ?pos4 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos4 (green ?new-value4))
    )
    (if (eq ?c3 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value4 (fact-slot-value ?pos4 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos4 (yellow ?new-value4))
    )
    (if (eq ?c3 black) then
        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value4 (fact-slot-value ?pos4 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos4 (black ?new-value4))
    )
    (if (eq ?c3 white) then
        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value4 (fact-slot-value ?pos4 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos4 (white ?new-value4))
    )
    (if (eq ?c3 purple) then
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value4 (fact-slot-value ?pos4 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos4 (purple ?new-value4))
    )
    (if (eq ?c3 orange) then
        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value4 (fact-slot-value ?pos4 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value4 (+ ?current-value4 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos4 (orange ?new-value4))
    )
    (modify ?control (counter 6))
    (printout t "Regola eseguita mp counter impostato a 6" crlf)
)

(defrule update-weights-mp4
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   ?control <- (control (counter 8))
   =>
    (bind ?weight-increment (* 1 ?mp))
    (printout t "Regola mp attivata con counter = 8" crlf)

    (if (eq ?c4 red) then
        (bind ?current-value1 (fact-slot-value ?pos1 red))
        (bind ?current-value2 (fact-slot-value ?pos2 red))
        (bind ?current-value3 (fact-slot-value ?pos3 red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (red ?new-value1))
        (modify ?pos2 (red ?new-value2))
        (modify ?pos3 (red ?new-value3))
    )
    (if (eq ?c4 blue) then
        (bind ?current-value1 (fact-slot-value ?pos1 blue))
        (bind ?current-value2 (fact-slot-value ?pos2 blue))
        (bind ?current-value3 (fact-slot-value ?pos3 blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (blue ?new-value1))
        (modify ?pos2 (blue ?new-value2))
        (modify ?pos3 (blue ?new-value3))
    )
    (if (eq ?c4 green) then
        (bind ?current-value1 (fact-slot-value ?pos1 green))
        (bind ?current-value2 (fact-slot-value ?pos2 green))
        (bind ?current-value3 (fact-slot-value ?pos3 green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (green ?new-value1))
        (modify ?pos2 (green ?new-value2))
        (modify ?pos3 (green ?new-value3))
    )
    (if (eq ?c4 yellow) then
        (bind ?current-value1 (fact-slot-value ?pos1 yellow))
        (bind ?current-value2 (fact-slot-value ?pos2 yellow))
        (bind ?current-value3 (fact-slot-value ?pos3 yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (yellow ?new-value1))
        (modify ?pos2 (yellow ?new-value2))
        (modify ?pos3 (yellow ?new-value3))
    )
    (if (eq ?c4 black) then
        (bind ?current-value1 (fact-slot-value ?pos1 black))
        (bind ?current-value2 (fact-slot-value ?pos2 black))
        (bind ?current-value3 (fact-slot-value ?pos3 black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (black ?new-value1))
        (modify ?pos2 (black ?new-value2))
        (modify ?pos3 (black ?new-value3))
    )
    (if (eq ?c4 white) then
        (bind ?current-value1 (fact-slot-value ?pos1 white))
        (bind ?current-value2 (fact-slot-value ?pos2 white))
        (bind ?current-value3 (fact-slot-value ?pos3 white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (white ?new-value1))
        (modify ?pos2 (white ?new-value2))
        (modify ?pos3 (white ?new-value3))
    )
    (if (eq ?c4 purple) then
        (bind ?current-value1 (fact-slot-value ?pos1 purple))
        (bind ?current-value2 (fact-slot-value ?pos2 purple))
        (bind ?current-value3 (fact-slot-value ?pos3 purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (purple ?new-value1))
        (modify ?pos2 (purple ?new-value2))
        (modify ?pos3 (purple ?new-value3))
    )
    (if (eq ?c4 orange) then
        (bind ?current-value1 (fact-slot-value ?pos1 orange))
        (bind ?current-value2 (fact-slot-value ?pos2 orange))
        (bind ?current-value3 (fact-slot-value ?pos3 orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (bind ?new-value2 (+ ?current-value2 ?weight-increment))
        (bind ?new-value3 (+ ?current-value3 ?weight-increment))
        (modify ?pos1 (orange ?new-value1))
        (modify ?pos2 (orange ?new-value2))
        (modify ?pos3 (orange ?new-value3))
    )
    (modify ?control (counter 7))
    (printout t "Regola eseguita mp counter impostato a 7" crlf)
)
(defrule update-weights-mmissing
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   ?control <- (control (counter 9))
   
   =>
   (bind ?missing (- 4 (+ ?rp ?mp)))
   (if (or (= ?missing 0) (= ?missing 4))
        then 
        (bind ?weight-increment (* 2 ?missing))
        (printout t "Regola mp attivata con counter = 5" crlf)

    (if (or (eq ?c1 red) (eq ?c2 red) (eq ?c3 red) (eq ?c4 red)) then
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
    )
    (if (or (eq ?c1 blue) (eq ?c2 blue) (eq ?c3 blue) (eq ?c4 blue)) then
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
    )
    (if (or (eq ?c1 green) (eq ?c2 green) (eq ?c3 green) (eq ?c4 green)) then
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
    )
    (if (or (eq ?c1 yellow) (eq ?c2 yellow) (eq ?c3 yellow) (eq ?c4 yellow)) then
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
    )
    (if (or (eq ?c1 black) (eq ?c2 black) (eq ?c3 black) (eq ?c4 black)) then
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
    )
    (if (or (eq ?c1 white) (eq ?c2 white) (eq ?c3 white) (eq ?c4 white)) then
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
    )
    (if (or (eq ?c1 purple) (eq ?c2 purple) (eq ?c3 purple) (eq ?c4 purple)) then
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
    )
    (if (or (eq ?c1 orange) (eq ?c2 orange) (eq ?c3 orange) (eq ?c4 orange)) then
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
    )
   )
   (if (= ?missing 4) then 
        (if (or (eq ?c1 red) (eq ?c2 red) (eq ?c3 red) (eq ?c4 red)) then
            (modify ?pos1 (red -99))
            (modify ?pos2 (red -99))
            (modify ?pos3 (red -99))
            (modify ?pos4 (red -99))
        )
        (if (or (eq ?c1 blue) (eq ?c2 blue) (eq ?c3 blue) (eq ?c4 blue)) then
            (modify ?pos1 (blue -99))
            (modify ?pos2 (blue -99))
            (modify ?pos3 (blue -99))
            (modify ?pos4 (blue -99))
        )
        (if (or (eq ?c1 green) (eq ?c2 green) (eq ?c3 green) (eq ?c4 green)) then
            (modify ?pos1 (green -99))
            (modify ?pos2 (green -99))
            (modify ?pos3 (green -99))
            (modify ?pos4 (green -99))
        )
        (if (or (eq ?c1 yellow) (eq ?c2 yellow) (eq ?c3 yellow) (eq ?c4 yellow)) then
            (modify ?pos1 (yellow -99))
            (modify ?pos2 (yellow -99))
            (modify ?pos3 (yellow -99))
            (modify ?pos4 (yellow -99))
        )
        (if (or (eq ?c1 black) (eq ?c2 black) (eq ?c3 black) (eq ?c4 black)) then
            (modify ?pos1 (black -99))
            (modify ?pos2 (black -99))
            (modify ?pos3 (black -99))
            (modify ?pos4 (black -99))
        )
        (if (or (eq ?c1 white) (eq ?c2 white) (eq ?c3 white) (eq ?c4 white)) then
            (modify ?pos1 (white -99))
            (modify ?pos2 (white -99))
            (modify ?pos3 (white -99))
            (modify ?pos4 (white -99))
        )
        (if (or (eq ?c1 purple) (eq ?c2 purple) (eq ?c3 purple) (eq ?c4 purple)) then
            (modify ?pos1 (purple -99))
            (modify ?pos2 (purple -99))
            (modify ?pos3 (purple -99))
            (modify ?pos4 (purple -99))
        )
        (if (or (eq ?c1 orange) (eq ?c2 orange) (eq ?c3 orange) (eq ?c4 orange)) then
            (modify ?pos1 (orange -99))
            (modify ?pos2 (orange -99))
            (modify ?pos3 (orange -99))
            (modify ?pos4 (orange -99))
        )
   )
   (if (= ?missing 0) then 
        (if (or (eq ?c1 red) (eq ?c2 red) (eq ?c3 red) (eq ?c4 red)) then
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
        )
        (if (or (eq ?c1 blue) (eq ?c2 blue) (eq ?c3 blue) (eq ?c4 blue)) then
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
        )
        (if (or (eq ?c1 green) (eq ?c2 green) (eq ?c3 green) (eq ?c4 green)) then
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
        )
        (if (or (eq ?c1 yellow) (eq ?c2 yellow) (eq ?c3 yellow) (eq ?c4 yellow)) then
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
        )
        (if (or (eq ?c1 black) (eq ?c2 black) (eq ?c3 black) (eq ?c4 black)) then
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
        )
        (if (or (eq ?c1 white) (eq ?c2 white) (eq ?c3 white) (eq ?c4 white)) then
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
        )
        (if (or (eq ?c1 purple) (eq ?c2 purple) (eq ?c3 purple) (eq ?c4 purple)) then
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
        )
        (if (or (eq ?c1 orange) (eq ?c2 orange) (eq ?c3 orange) (eq ?c4 orange)) then
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
        )

   )
   (modify ?control (counter 8))
    (printout t "Regola eseguita miss counter impostato a 8" crlf)
)
