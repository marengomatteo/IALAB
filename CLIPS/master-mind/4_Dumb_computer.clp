
(defmodule DUMB_COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate combination (slot step) (multislot colors))

(deftemplate peso (slot pos) (slot red) (slot blue) (slot yellow) (slot white) (slot black) (slot purple) (slot green) (slot orange))

(deffacts pesi
  (peso (pos 1) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 2) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 3) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
  (peso (pos 4) (red 0) (blue 0) (yellow 0) (white 0) (black 0) (purple 0) (green 0) (orange 0))
)


(defrule init-computer
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

; (defrule computer-player
;   (status (step ?s) (mode computer))
;   (guess (step ?prev-s) (g $?k))
;   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
;   (test (= ?prev-s (- ?s 1)))
;   =>
 
;   ;; Inizializza la lista dei colori disponibili
;   (bind ?colors (create$ red blue green yellow orange white black purple))
;   ;; Inizializza la lista del nuovo tentativo
;   (bind ?new-guess (create$))
;   ;; Genera il tentativo automaticamente
;   (loop-for-count (?i 4)
;     (bind ?random-color (nth$ (random 1 (length$ ?colors)) ?colors)) ;; sceglie randomicamente un colore
;     (bind ?colors (delete-member$ ?colors ?random-color)) 
;     ;; Rimuove il colore dalla lista dei colori disponibili così non può generare il tentativo con due colori uguali
;     (bind ?new-guess (create$ ?new-guess ?random-color)) ;; aggiunge a new guess il colore scelto
;   )
;   ;; Inserisce il nuovo tentativo
;   (assert (guess (step ?s) (g (nth$ 1 ?new-guess) 
;                                (nth$ 2 ?new-guess) 
;                                (nth$ 3 ?new-guess) 
;                                (nth$ 4 ?new-guess))))
;   ;; stampa il tentativo      
;   (printout t "Computer's guess at step " ?s ": " crlf) 
;   (printout t (implode$ ?new-guess) crlf)
;   (pop-focus)
; )

(defrule update-weights-rp-pos1
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 1))
   =>
    (bind ?weight-increment (* 2 ?rp))

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
)

(defrule update-weights-rp-pos2
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 $?colors))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   ?pos <- (peso (pos 2))
   =>
    (bind ?weight-increment (* 2 ?rp))

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
)

; (defrule update-weights-rp-pos3
;    (status (step ?s) (mode computer))
;    (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
;    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
;    (test (= ?prev-s (- ?s 1)))
;    ?pos <- (peso (pos 3))
;    =>
;     (bind ?weight-increment (* 2 ?rp))

;     (if (eq ?c3 red) then
;         (bind ?current-value1 (fact-slot-value ?pos red))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (red ?new-value1))
;     )
;     (if (eq ?c3 blue) then
;         (bind ?current-value1 (fact-slot-value ?pos blue))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (blue ?new-value1))
;     )
;     (if (eq ?c3 green) then
;         (bind ?current-value1 (fact-slot-value ?pos green))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (green ?new-value1))
;     )
;     (if (eq ?c3 yellow) then
;         (bind ?current-value1 (fact-slot-value ?pos yellow))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (yellow ?new-value1))
;     )
;     (if (eq ?c3 black) then
;         (bind ?current-value1 (fact-slot-value ?pos black))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (black ?new-value1))
;     )
;     (if (eq ?c3 white) then
;         (bind ?current-value1 (fact-slot-value ?pos white))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (white ?new-value1))
;     )
;     (if (eq ?c3 purple) then
;         (bind ?current-value1 (fact-slot-value ?pos purple))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (purple ?new-value1))
;     )
;     (if (eq ?c3 orange) then
;         (bind ?current-value1 (fact-slot-value ?pos orange))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (orange ?new-value1))
;     )
; (pop-focus)
; )
; (defrule update-weights-rp-pos4
;    (status (step ?s) (mode computer))
;    (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
;    (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
;    (test (= ?prev-s (- ?s 1)))
;    ?pos <- (peso (pos 4))
;    =>
;     (bind ?weight-increment (* 2 ?rp))

;     (if (eq ?c4 red) then
;         (bind ?current-value1 (fact-slot-value ?pos red))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (red ?new-value1))
;     )
;     (if (eq ?c4 blue) then
;         (bind ?current-value1 (fact-slot-value ?pos blue))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (blue ?new-value1))
;     )
;     (if (eq ?c4 green) then
;         (bind ?current-value1 (fact-slot-value ?pos green))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (green ?new-value1))
;     )
;     (if (eq ?c4 yellow) then
;         (bind ?current-value1 (fact-slot-value ?pos yellow))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (yellow ?new-value1))
;     )
;     (if (eq ?c4 black) then
;         (bind ?current-value1 (fact-slot-value ?pos black))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (black ?new-value1))
;     )
;     (if (eq ?c4 white) then
;         (bind ?current-value1 (fact-slot-value ?pos white))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (white ?new-value1))
;     )
;     (if (eq ?c4 purple) then
;         (bind ?current-value1 (fact-slot-value ?pos purple))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (purple ?new-value1))
;     )
;     (if (eq ?c4 orange) then
;         (bind ?current-value1 (fact-slot-value ?pos orange))
;         (bind ?new-value1 (+ ?current-value1 ?weight-increment))
;         (modify ?pos (orange ?new-value1))
;     )
;     (pop-focus)
; )


(defrule check-rp 
   (status (step ?s) (mode computer))
   (guess (step ?prev-s) (g ?c1 ?c2 ?c3 ?c4))
   (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
   (test (= ?prev-s (- ?s 1)))
   =>


   (pop-focus)
)



