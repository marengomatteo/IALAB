(deftemplate permutation
   (multislot values))


(deftemplate guess
	(slot step (type INTEGER))
	(multislot g (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deffacts permutations
   (permutation (values purple black green yellow))
   (permutation (values purple black green red))
   (permutation (values purple black green blue))
   (permutation (values purple black blue white))
   (permutation (values yellow purple blue orange))
   (permutation (values purple black blue yellow))
   (permutation (values purple black blue red))
   (permutation (values purple black blue green))
   (permutation (values purple white black orange))
   (permutation (values blue green orange purple))
   (permutation (values purple white black yellow))
   (permutation (values purple white black red))
   (permutation (values purple white black green))
   (permutation (values purple white black blue))
   (permutation (values purple white orange black))
   (permutation (values purple white orange yellow))
   (permutation (values purple white orange red))
   (permutation (values purple white orange green))
   (permutation (values purple white orange blue))
   (permutation (values purple white yellow black))
   )

;;; eliminare fino a qui

(deffacts initial-flag
   (first-permutation-found no))

(defrule find-first-permutation (declare (salience 10))
  ?flag <- (first-permutation-found no)
  ?f <- (permutation (values yellow $?values))
=>
   (retract ?flag)
   (assert (first-permutation-found yes))
   (assert (guess (step 0) (g $?values)))
   (printout t "First permutation: " $?values crlf)
)
(defrule guess-feedback (declare (salience 1))
  ?guess <- (guess (step ?prev-s)(g ?g1 ?g2 ?g3 ?g4))
  (permutation (values ?p1 ?p2 ?p3 ?p4))
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
    (retract (permutation (values ?p1 ?p2 ?p3 ?p4)))
  )
)
