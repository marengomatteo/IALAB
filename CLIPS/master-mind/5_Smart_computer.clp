
(defmodule COMPUTER (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate color
   (slot value))

(deftemplate permutation
   (multislot values))

(deffacts initial
    (k-combination 4)
    (color (value blue))
    (color (value green))
    (color (value red))
    (color (value yellow))
    (color (value orange))
    (color (value white))
    (color (value black))
    (color (value purple))
)

(defrule first-in-permutation (declare (salience 100))
   (k-combination ~0)
   (color (value ?color))
   =>
   (assert (permutation (values ?color))))


(defrule next-in-permutation (declare (salience 100))
   (k-combination ?k)
   ?p <- (permutation (values $?colors))
   (test (< (length$ ?colors) ?k))
   (color (value ?color))
   (test (not (member$ ?color ?colors)))
   =>
   (assert (permutation (values ?colors ?color))))


(defrule cleanup
   (declare (salience 95))
   (k-combination ?k)
   ?p <- (permutation (values $?colors))
   (test (< (length$ ?colors) ?k))
   =>
   (retract ?p))


(defrule init-computer (declare (salience 10))
  (status (step 0) (mode computer))
  ?p <- (permutation (values $?colors))
  =>
  ;; Inizializza la lista del nuovo tentativo
  ;; Inserisce il nuovo tentativo
  (assert (guess (step 0) (g $?colors)))
  ;; stampa il tentativo      
  (printout t "Computer's guess at step " 0 ": " crlf) 
  (printout t (implode$ ?colors) crlf)
  (pop-focus)
)

(defrule guess-feedback (declare (salience 5))
  ?guess <- (guess (step ?prev-s)(g ?g1 ?g2 ?g3 ?g4))
  ?permutations <- (permutation (values ?p1 ?p2 ?p3 ?p4))
  (status (step ?s) (mode computer))
  (answer (step ?prev-s) (right-placed ?rpOld) (miss-placed ?mpOld))
  (test (= ?prev-s (- ?s 1)))
  =>
  (bind ?rp 0)
  (bind ?mp 0)

  ; Calcolo di rp (right position)
  (if (eq ?g1 ?p1) then (bind ?rp (+ ?rp 1)))
  (if (eq ?g2 ?p2) then (bind ?rp (+ ?rp 1)))
  (if (eq ?g3 ?p3) then (bind ?rp (+ ?rp 1)))
  (if (eq ?g4 ?p4) then (bind ?rp (+ ?rp 1)))

  ; Calcolo di mp (misplaced position)
  ; Lista di appoggio per i colori della permutazione
  (bind ?perms (create$ ?p1 ?p2 ?p3 ?p4))
  (if (and (neq ?g1 ?p1) (member$ ?g1 ?perms)) then (bind ?mp (+ ?mp 1)))
  (if (and (neq ?g2 ?p2) (member$ ?g2 ?perms)) then (bind ?mp (+ ?mp 1)))
  (if (and (neq ?g3 ?p3) (member$ ?g3 ?perms)) then (bind ?mp (+ ?mp 1)))
  (if (and (neq ?g4 ?p4) (member$ ?g4 ?perms)) then (bind ?mp (+ ?mp 1)))

  ; Confronto del feedback calcolato con il feedback atteso
  (if (not (and (eq ?rp ?rpOld) (eq ?mp ?mpOld))) then
    (retract ?permutations)
  )
)

(defrule computer-player (declare (salience 1))
   (status (step ?s) (mode computer))
   ?new-guess <- (permutation (values $?colors))
   (not (guess (g $?colors)))
=>
   (assert (guess (step ?s) (g $?colors)))
   ;; stampa il tentativo 
   (printout t crlf)    
   (printout t "Computer's guess at step " ?s ": " crlf) 
   (printout t (implode$ ?colors) crlf)
   (pop-focus)
)