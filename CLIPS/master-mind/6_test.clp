;buonasera amici questo è un testamento da parte di Fratta. Spero di trovarvi bene (andre dagliela a matte).
;cosa ho fatto? tutte le condizioni di if per ogni colore delle posizioni sono diventate regole separate. 
; Così al posto di entrare in una regola e fare mille if entra solo nella regola del colore
; facendo così non servirà più fare (eq ?c1 red) ma basterà, nella prima parte della regola, mettere "red"
;  dove c'è (g ?c1 ?c2 ?c3 ?c4). Ho poi creato delle variabili globali per modificare i pesi dei vari ?rp ?mp ?missing
;tramite salience bisogna far si che vengano chiamate in ordine: primop tentativo -> aggiornamento pesi ->secondo tentativo etc. 
; per fare ciò ho messo un salience crescente in modo che vengano chiamate in ordine, ma rimangono in loop.
; TODO: sistemare la roba delle chiamate che rimangono in loop. Spostare il modo per trovare il massimo in una funzione a parte.
; TODO: inserire le guess in una pool. Se sto tentando di rifare la guess già fatta allora scombino l'ordine. 
; NOTA BENE: attualmente ho mesos 9 regole che asseriscono random e solo la decima fa un'asserzione sulla base dei pesi. 
; Questa cosa è solo per testare ma mi ha detto miky che possiamo usarla come versione stupida
; bacini
(defmodule DUMB_COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))


(defglobal ?*pesoRP* = 2)
(defglobal ?*pesoMP* = 1)
(defglobal ?*pesoMISSING* = 2)


(deftemplate combination (slot step) (multislot colors))

(deftemplate peso (slot pos) (slot red) (slot blue) (slot yellow) (slot white) (slot black) (slot purple) (slot green) (slot orange))

(deffacts pesi
  (peso (pos 1) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 2) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 3) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 4) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
)



(defrule init-computer (declare (salience 100))
  (status (step 0) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer2 (declare (salience 3))
  (status (step 1) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer3 (declare (salience 3))
  (status (step 2) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer4 (declare (salience 3))
  (status (step 3) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer5 (declare (salience 3))
  (status (step 4) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer6 (declare (salience 3))
  (status (step 5) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer7 (declare (salience 3))
  (status (step 6) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer8 (declare (salience 3))
  (status (step 7) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule init-computer9 (declare (salience 3))
  (status (step 8) (mode computer))
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
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)

(defrule computer-player (declare (salience 1))
  (status (step ?s) (mode computer))
  (guess (step ?prev-s) (g $?k))
  (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
  (test (= ?prev-s (- ?s 1)))
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
  
  ;; Stampa il tentativo
  (printout t "Computer's guess at step " ?s ": " crlf)
  (printout t ?max-color1 " " ?max-color2 " " ?max-color3 " " ?max-color4 crlf)
  (pop-focus)
)

; sostituire c1 con red
(defrule update-weights-rp-pos1-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
        (printout t "Regola eseguite, Red in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    
    (printout t "Regola eseguite, Blue in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    
    (printout t "Regola eseguite, Green in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
  
    (printout t "Regola eseguite, Yellow in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
 
    (printout t "Regola eseguite, Black in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
  
    (printout t "Regola eseguite, White in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))

    (printout t "Regola eseguite, Purple in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos1-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))

    (printout t "Regola eseguite, Orange in posizione 1 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
  
    (printout t "Regola eseguite, Red in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
   
    (printout t "Regola eseguite, Blue in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
  
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
   
    (printout t "Regola eseguite, Green in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   

        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
  
    (printout t "Regola eseguite, Yellow in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
  
    (printout t "Regola eseguite, Black in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))

    (printout t "Regola eseguite, White in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    
    (printout t "Regola eseguite, Purple in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos2-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
 
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
  
    (printout t "Regola eseguite, Orange in posizione 2 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    

        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))

    (printout t "Regola eseguite, Red in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
    
    (printout t "Regola eseguite, Blue in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
        
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
    
    (printout t "Regola eseguite, Green in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
 
    (bind ?current-value1 (fact-slot-value ?pos yellow))
    (bind ?new-value1 (+ ?current-value1 ?weight-increment))
    (modify ?pos (yellow ?new-value1))

    (printout t "Regola eseguite, Yellow in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    
    (printout t "Regola eseguite, Black in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
   
    (printout t "Regola eseguite, White in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
  
    (printout t "Regola eseguite, Purple in posizione 3 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos3-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 3))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
    
    (printout t "Regola eseguite, Orange in posizione 3 ha peso " ?new-value1 crlf)
)


(defrule update-weights-rp-pos4-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
   
        (bind ?current-value1 (fact-slot-value ?pos red))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (red ?new-value1))
     
    (printout t "Regola eseguite, Red in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos blue))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (blue ?new-value1))
   
    (printout t "Regola eseguite, Blue in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos green))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (green ?new-value1))
     
    (printout t "Regola eseguite, Green in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos yellow))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (yellow ?new-value1))
    
    (printout t "Regola eseguite, Yellow in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos black))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (black ?new-value1))
    
    (printout t "Regola eseguite, Black in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos white))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (white ?new-value1))
     
    (printout t "Regola eseguite, White in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos purple))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (purple ?new-value1))
    
    (printout t "Regola eseguite, Purple in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-rp-pos4-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 4))
   =>
    (bind ?weight-increment (* ?*pesoRP* ?rp))
    
        (bind ?current-value1 (fact-slot-value ?pos orange))
        (bind ?new-value1 (+ ?current-value1 ?weight-increment))
        (modify ?pos (orange ?new-value1))
     
    (printout t "Regola eseguite, Orange in posizione 4 ha peso " ?new-value1 crlf)
)

(defrule update-weights-mp1-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
    
    (printout t "Regola eseguite, Red è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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

    (printout t "Regola eseguite, Blue è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
  
    (printout t "Regola eseguite, Green è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
  
    (printout t "Regola eseguite, Yellow è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
   
    (printout t "Regola eseguite, Black è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
  
    (printout t "Regola eseguite, White è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
   
    (printout t "Regola eseguite, Purple è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp1-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
   
    (printout t "Regola eseguite, Orange è stato incrementato di  " ?weight-increment crlf)
)



(defrule update-weights-mp2-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 red $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
    
    (printout t "Regola eseguite, Red è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 blue $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
      
    (printout t "Regola eseguite, Blue è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 green $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
       
    (printout t "Regola eseguite, Green è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 yellow $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
    
    (printout t "Regola eseguite, Yellow è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 black $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
     
    (printout t "Regola eseguite, Black è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 white $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
      
    (printout t "Regola eseguite, White è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 purple $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
      
    (printout t "Regola eseguite, Purple è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp2-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 orange $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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
     
    (printout t "Regola eseguite, Orange è stato incrementato di  " ?weight-increment crlf)
)


(defrule update-weights-mp3-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
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

    (printout t "Regola eseguite, Red è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
  
    (printout t "Regola eseguite, Blue è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
   
    (printout t "Regola eseguite, Green è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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

    (printout t "Regola eseguite, Yellow è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
  
    (printout t "Regola eseguite, Black è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
  
    (printout t "Regola eseguite, White è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
 
    (printout t "Regola eseguite, Purple è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp3-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
   
    (printout t "Regola eseguite, Orange è stato incrementato di  " ?weight-increment crlf)
)


(defrule update-weights-mp4-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
   
    (printout t "Regola eseguite, Red è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
 
    (printout t "Regola eseguite, Blue è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
 
    (printout t "Regola eseguite, Green è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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

    (printout t "Regola eseguite, Yellow è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
   
    (printout t "Regola eseguite, Black è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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

    (printout t "Regola eseguite, White è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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

    (printout t "Regola eseguite, Purple è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-mp4-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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

    (printout t "Regola eseguite, Orange è stato incrementato di  " ?weight-increment crlf)
)

(defrule update-weights-missing-red (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing red ridotto di" ?weight-increment crlf)
)
(defrule update-weights-missing-blue (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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

    (printout t "Regola eseguita Missing blue ridotto di" ?weight-increment crlf)
)
    

(defrule update-weights-missing-green (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing green ridotto di" ?weight-increment crlf)
)

(defrule update-weights-missing-yellow (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing yellow ridotto di" ?weight-increment crlf)
    )

(defrule update-weights-missing-black (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing black ridotto di" ?weight-increment crlf)
    )

(defrule update-weights-missing-white (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing white ridotto di" ?weight-increment crlf)
    )

(defrule update-weights-missing-purple (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing purple ridotto di" ?weight-increment crlf)
    )
(defrule update-weights-missing-orange (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   (test (not (or (= (- 4 (+ ?rp ?mp)) 4) (= (- 4 (+ ?rp ?mp)) 0))))
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
    (printout t "Regola eseguita Missing orange ridotto di" ?weight-increment crlf)
    )

(defrule update-weights-missing-red-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>

            (modify ?pos1 (red -99))
            (modify ?pos2 (red -99))
            (modify ?pos3 (red -99))
            (modify ?pos4 (red -99))
         
    (printout t "Regola eseguita Red mancante impostato a -99" crlf)
)

(defrule update-weights-missing-blue-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (blue -99))
            (modify ?pos2 (blue -99))
            (modify ?pos3 (blue -99))
            (modify ?pos4 (blue -99))
             
    (printout t "Regola eseguita Blue mancante impostato a -99" crlf)
)

(defrule update-weights-missing-green-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
  
            (modify ?pos1 (green -99))
            (modify ?pos2 (green -99))
            (modify ?pos3 (green -99))
            (modify ?pos4 (green -99))
            
    (printout t "Regola eseguita Green mancante impostato a -99" crlf)
)
(defrule update-weights-missing-yellow-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (yellow -99))
            (modify ?pos2 (yellow -99))
            (modify ?pos3 (yellow -99))
            (modify ?pos4 (yellow -99))
            
    (printout t "Regola eseguita Yellow mancante impostato a -99" crlf)
)
(defrule update-weights-missing-black-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (black -99))
            (modify ?pos2 (black -99))
            (modify ?pos3 (black -99))
            (modify ?pos4 (black -99))
            
    (printout t "Regola eseguita Black mancante impostato a -99" crlf)
)
(defrule update-weights-missing-white-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>

            (modify ?pos1 (white -99))
            (modify ?pos2 (white -99))
            (modify ?pos3 (white -99))
            (modify ?pos4 (white -99))
            
    (printout t "Regola eseguita White mancante impostato a -99" crlf)
)

(defrule update-weights-missing-purple-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
            (modify ?pos1 (purple -99))
            (modify ?pos2 (purple -99))
            (modify ?pos3 (purple -99))
            (modify ?pos4 (purple -99))
            
    (printout t "Regola eseguita Purple mancante impostato a -99" crlf)
)
(defrule update-weights-missing-orange-none (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colorsbefore orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
   (test (= (+ ?rp ?mp) 0))
   =>
        
            (modify ?pos1 (orange -99))
            (modify ?pos2 (orange -99))
            (modify ?pos3 (orange -99))
            (modify ?pos4 (orange -99))
            
    (printout t "Regola eseguita Orange mancante impostato a -99" crlf)
)
   

(defrule update-weights-missing-red-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors red $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
        
    (printout t "Regola eseguita Red presente aumentato di 99" crlf)
)
(defrule update-weights-missing-blue-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors blue $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
        
    (printout t "Regola eseguita Blue presente aumentato di 99" crlf)
)
(defrule update-weights-missing-green-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors green $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
        
    (printout t "Regola eseguita Green presente aumentato di 99" crlf)
)

(defrule update-weights-missing-yellow-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors yellow $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
        
    (printout t "Regola eseguita Yellow presente aumentato di 99" crlf)
)
(defrule update-weights-missing-black-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors black $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
        
    (printout t "Regola eseguita Black presente aumentato di 99" crlf)
)
(defrule update-weights-missing-white-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors white $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
    (printout t "Regola eseguita White presente aumentato di 99" crlf)
)
(defrule update-weights-missing-purple-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors purple $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
    (printout t "Regola eseguita Purple presente aumentato di 99" crlf)
)
(defrule update-weights-missing-orange-all (declare (salience 10))
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g $?colors orange $?colorsafter))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos1 <- (peso (pos 1))
   ?pos2 <- (peso (pos 2))
   ?pos3 <- (peso (pos 3))
   ?pos4 <- (peso (pos 4))
   
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
    (printout t "Regola eseguita Orange presente aumentato di 99" crlf)
)

