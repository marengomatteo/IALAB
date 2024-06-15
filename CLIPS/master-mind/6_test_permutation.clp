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
   (permutation (values purple white black yellow))
   (permutation (values purple white black red))
   (permutation (values purple white black green))
   (permutation (values purple white black blue))
   (permutation (values purple white orange black))
   (permutation (values purple white orange yellow))
   (permutation (values purple white orange red))
   (permutation (values purple white orange green))
   (permutation (values purple white orange blue))
   (permutation (values purple white yellow black)))

;;; eliminare fino a qui

(deffacts initial-flag
   (first-permutation-found no))

(defrule find-first-permutation
  ?flag <- (first-permutation-found no)
  ?f <- (permutation (values yellow $?values))
=>
   (retract ?flag)
   (assert (first-permutation-found yes))
   (assert (guess (step 0) (g $?values)))
   (printout t "First permutation: " $?values crlf)
   (halt)
)
