/*
    azione applicabile:
        - controlliamo che non sia occupata da un muro fisso
        - controlliamo che non sia presente del ghiaccio
*/
/* per applicabile controllo anche che il ghiaccio sia abbattibile o no, quindi se ho il martello è applicabile*/

occupata(pos(R,C)) :- 
    muro(pos(R,C)).

occupata(pos(R,C)) :-
    gemma(pos(R,C)).

occupata(pos(R,C)) :-
    \+possiede(martello),
    ghiaccio(pos(R,C)).


applicabile(nord,pos(R,C)) :- 
    R>1,
    R1 is R-1,
    \+ occupata(pos(R1,C)).

applicabile(sud,pos(R,C)) :- 
    num_righe(NR), R<NR,
    R1 is R+1,
    \+ occupata(pos(R1,C)).

applicabile(ovest,pos(R,C)) :- 
    C>1,
    C1 is C-1,
    \+ occupata(pos(R,C1)).

applicabile(est,pos(R,C)) :- 
    num_col(NC), C<NC,
    C1 is C+1,
    \+ occupata(pos(R,C1)).

trasforma(est,pos(R,C),pos(R,C1)) :- 
    applicabile(est, pos(R,C+1)),
    C1 is C+1,
    trasforma(est,pos(R,C), pos(R,C1)).

trasforma(ovest,pos(R,C),pos(R,C1)) :- 
    applicabile(ovest, pos(R,C-1)),
    C1 is C-1,
    trasforma(ovest,pos(R,C), pos(R,C1)).

trasforma(sud,pos(R,C),pos(R1,C)) :- 
    applicabile(sud, pos(R1,C)),
    R1 is R+1,
    trasforma(sud,pos(R,C), pos(R1,C)).

trasforma(nord,pos(R,C),pos(R1,C)) :- 
    applicabile(nord, pos(R1,C)),
    R1 is R-1,
    trasforma(nord,pos(R,C), pos(R1,C)).

raccogli(pos(R,C)) :-
    martello(pos(R,C)),
    \+ possiede(martello),
    assert(possiede(martello)).


muovi_gemme :-
    findall(pos(R,C), gemma(pos(R,C)), Gemme), # Trova tuttel el pos delle gemme
    muovi_lista_gemme(Gemme).

muovi_lista_gemme([]). #Se non ci sono più gemme da muovere, termina
muovi_lista_gemme(nord, [pos(R,C)|Tail]) :-
    trasforma(nord,pos(R,C), pos(R1,C))
    retract(gemma(pos(R,C))), #Possiamo usare il retract?? Altrimenti dobbiamo tenere traccia della lista di posizioni delle gemme e passarla come parametro a ogni chiamata
    assert(gemma(pos(R1,C))),
    muovi_lista_gemme(Rest).

#Verifica per il bonus
tre_gemme_adiacenti([G1, G2, G3]) :-
    adiacenti(G1, G2),  % Controlla se G1 è adiacente a G2
    adiacenti(G2, G3),  % Controlla se G2 è adiacente a G3
    adiacenti(G1, G3).  % Controlla se G1 è adiacente a G3

adiacenti(pos(R1,C1), pos(R2,C2)) :-
    D_R is abs(R1 - R2),
    D_C is abs(C1 - C2),
    D_R =:= 1,
    D_C =:= 0.

adiacenti(pos(R1,C1), pos(R2,C2)) :-
    D_R is abs(R1 - R2),
    D_C is abs(C1 - C2),
    D_R =:= 0,
    D_C =:= 1.
    # non so se possiamo usare l'or  (   (D_R =:= 1, D_C =:= 0) ; (D_R =:= 0, D_C =:= 1) ).
