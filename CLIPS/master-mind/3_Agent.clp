;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )

(defrule computer-player
  (status (step ?s) (mode computer))
  (not (guess (step ?s)))
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
  (printout t "Computer's guess at step " ?s ": " (implode$ ?new-guess) crlf) 
  (pop-focus)
)
  
;; l'agente deve scegliere 4 colori tra quelli indicati nel game
;; in base alla risposta dovrà scegliere altri 4 colori
;; dovrà scrivere i colori a terminale


; TENTATIVI MIGLIORI:
; ------------ 1 -----------------
; Computer's guess at step 0: yellow purple orange blue
; Right placed 1 missplaced 2
; Computer's guess at step 1: red white green orange
; Right placed 0 missplaced 2
; Computer's guess at step 2: black red blue purple
; Right placed 1 missplaced 1
; Computer's guess at step 3: blue purple yellow green
; Right placed 1 missplaced 2
; Computer's guess at step 4: green blue black purple
; Right placed 1 missplaced 2
; Computer's guess at step 5: purple orange green blue
; Right placed 0 missplaced 4 --
; Computer's guess at step 6: purple yellow black red
; Right placed 0 missplaced 1
; Computer's guess at step 7: white green orange purple
; Right placed 3 missplaced 0 --
; Computer's guess at step 8: yellow white purple red
; Right placed 0 missplaced 1
; Computer's guess at step 9: white orange purple green
; Right placed 0 missplaced 3
; GAME OVER!! 
; The secret code was: (blue green orange purple)
 
; ------------- 2 ---------------
; Computer's guess at step 0: yellow red black green
; Right placed 0 missplaced 1
; Computer's guess at step 1: blue green orange purple
; You have discovered the secrete code!


