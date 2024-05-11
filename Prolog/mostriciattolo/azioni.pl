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