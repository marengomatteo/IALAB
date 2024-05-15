(deftemplate codice
    (slot colore1)
    (slot colore2)
    (slot colore3)
    (slot colore4))

(deftemplate tentativo
    (slot colore1)
    (slot colore2)
    (slot colore3)
    (slot colore4))


(deffacts colori-ammessi
    (colore blue)
    (colore green)
    (colore red)
    (colore yellow)
    (colore orange)
    (colore white)
    (colore black)
    (colore purple))

(defrule inizia-gioco
    =>
    (printout t "Benvenuto a Mastermind! Indovina il codice segreto composto da 4 colori diversi tra loro." crlf)
    (assert (codice (colore1 ?) (colore2 ?) (colore3 ?) (colore4 ?)))
    (printout t "I colori ammessi sono: blue, green, red, yellow, orange, white, black, purple." crlf)
    (printout t "Inserisci il tuo primo tentativo separando i colori da uno spazio (es. blue green red yellow)." crlf))

