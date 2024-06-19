
(defmodule DUMB_COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

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

(defrule computer-player
  (status (step ?s) (mode computer))
  (guess (step ?prev-s) (g $?k))
  (answer (step ?prev-s) (right-placed ?rp) (miss-placed ?mp))
  (test (= ?prev-s (- ?s 1)))
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
  (assert (guess (step ?s) (g (nth$ 1 ?new-guess) 
                               (nth$ 2 ?new-guess) 
                               (nth$ 3 ?new-guess) 
                               (nth$ 4 ?new-guess))))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " ?s ": " crlf) 
  (printout t (implode$ ?new-guess) crlf)
  (pop-focus)
)
